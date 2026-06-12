-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.java.syntax

module Hydra.Dsl.Java.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for the hydra.java.syntax.AdditionalBound wrapper
additionalBound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
additionalBound x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AdditionalBound"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the minus variant of hydra.java.syntax.AdditiveExpression
additiveExpressionMinus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
additiveExpressionMinus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the plus variant of hydra.java.syntax.AdditiveExpression
additiveExpressionPlus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
additiveExpressionPlus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.AdditiveExpression
additiveExpressionUnary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
additiveExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_Binary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
additiveExpression_Binary lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
additiveExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
additiveExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
additiveExpression_BinaryWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
additiveExpression_BinaryWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.AmbiguousName wrapper
ambiguousName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ambiguousName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AmbiguousName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.AndExpression wrapper
andExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
andExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AndExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotatedIdentifier annotations identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotatedIdentifierAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotatedIdentifierIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotatedIdentifierWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotatedIdentifierWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.AnnotationInterfaceBody wrapper
annotationInterfaceBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
annotationInterfaceDeclaration modifiers identifier body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceDeclarationWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5
annotationInterfaceElementDeclaration modifiers type_ identifier dims default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the default field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceElementDeclarationDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceElementDeclarationDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceElementDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceElementDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceElementDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceElementDeclarationWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the dims field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceElementDeclarationWithDims original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceElementDeclarationWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceElementDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
annotationInterfaceElementDeclarationWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the abstract variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAbstract :: Typed.TypedTerm t0
annotationInterfaceElementModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceElementModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the public variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierPublic :: Typed.TypedTerm t0
annotationInterfaceElementModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotationInterface variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationAnnotationInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceMemberDeclarationAnnotationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceMemberDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constant variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationConstant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceMemberDeclarationConstant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationInterfaceMemberDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the marker variant of hydra.java.syntax.Annotation
annotationMarker :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationMarker x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "marker"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.java.syntax.Annotation
annotationNormal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the singleElement variant of hydra.java.syntax.Annotation
annotationSingleElement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotationSingleElement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleElement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayAccess
arrayAccess :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayAccess expression variant =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm variant)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.ArrayAccess
arrayAccessExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayAccessExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ArrayAccess
arrayAccessVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayAccessVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.ArrayAccess
arrayAccessWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayAccessWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.ArrayAccess
arrayAccessWithVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayAccessWithVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the arrayCreationWithInitializer variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantArrayCreationWithInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayAccess_VariantArrayCreationWithInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayCreationWithInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayAccess_VariantName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayAccess_VariantPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withInitializer variant of hydra.java.syntax.ArrayCreationExpression
arrayCreationExpressionWithInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializerClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerPrimitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializerPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
arrayCreationExpressionWithInitializer_ClassOrInterface type_ dims array =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Typed.unTypedTerm array)}]}))
-- | DSL accessor for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializer_ClassOrInterfaceArray x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "array")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializer_ClassOrInterfaceDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializer_ClassOrInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithArray original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithDims original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "array")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "array")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_Primitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
arrayCreationExpressionWithInitializer_Primitive type_ dims array =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Typed.unTypedTerm array)}]}))
-- | DSL accessor for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializer_PrimitiveArray x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "array")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializer_PrimitiveDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithInitializer_PrimitiveType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithInitializer_PrimitiveWithArray original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithInitializer_PrimitiveWithDims original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "array")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithInitializer_PrimitiveWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "array")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the withoutInitializer variant of hydra.java.syntax.ArrayCreationExpression
arrayCreationExpressionWithoutInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializerClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerPrimitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializerPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
arrayCreationExpressionWithoutInitializer_ClassOrInterface type_ dimExprs dims =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Typed.unTypedTerm dimExprs)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm dims)}]}))
-- | DSL accessor for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDimExprs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDimExprs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "dimExprs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDimExprs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDimExprs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDims original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_Primitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
arrayCreationExpressionWithoutInitializer_Primitive type_ dimExprs dims =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Typed.unTypedTerm dimExprs)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm dims)}]}))
-- | DSL accessor for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveDimExprs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializer_PrimitiveDimExprs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "dimExprs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializer_PrimitiveDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayCreationExpressionWithoutInitializer_PrimitiveType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithDimExprs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithoutInitializer_PrimitiveWithDimExprs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithoutInitializer_PrimitiveWithDims original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayCreationExpressionWithoutInitializer_PrimitiveWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.ArrayInitializer wrapper
arrayInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ArrayInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.ArrayType
arrayType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayType dims variant =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm variant)}]}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayType
arrayTypeDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayTypeDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ArrayType
arrayTypeVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayTypeVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayType
arrayTypeWithDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayTypeWithDims original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.ArrayType
arrayTypeWithVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
arrayTypeWithVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayType_VariantClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantPrimitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayType_VariantPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantVariable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
arrayType_VariantVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pair variant of hydra.java.syntax.AssertStatement
assertStatementPair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assertStatementPair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the single variant of hydra.java.syntax.AssertStatement
assertStatementSingle :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assertStatementSingle x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.AssertStatement_Pair
assertStatement_Pair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
assertStatement_Pair first second =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Typed.unTypedTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Typed.unTypedTerm second)}]}))
-- | DSL accessor for the first field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairFirst :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assertStatement_PairFirst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the second field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairSecond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assertStatement_PairSecond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the first field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairWithFirst :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
assertStatement_PairWithFirst original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
              Core.projectionFieldName = (Core.Name "second")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the second field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairWithSecond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
assertStatement_PairWithSecond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.Assignment
assignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
assignment lhs op expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.Assignment
assignmentExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.java.syntax.AssignmentExpression
assignmentExpressionAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentExpressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the conditional variant of hydra.java.syntax.AssignmentExpression
assignmentExpressionConditional :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentExpressionConditional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the lhs field of hydra.java.syntax.Assignment
assignmentLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.java.syntax.Assignment
assignmentOp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the and variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorAnd :: Typed.TypedTerm t0
assignmentOperatorAnd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the div variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorDiv :: Typed.TypedTerm t0
assignmentOperatorDiv =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minus variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorMinus :: Typed.TypedTerm t0
assignmentOperatorMinus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mod variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorMod :: Typed.TypedTerm t0
assignmentOperatorMod =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the or variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorOr :: Typed.TypedTerm t0
assignmentOperatorOr =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorPlus :: Typed.TypedTerm t0
assignmentOperatorPlus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftLeft variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftLeft :: Typed.TypedTerm t0
assignmentOperatorShiftLeft =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftRight variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftRight :: Typed.TypedTerm t0
assignmentOperatorShiftRight =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftRightZeroFill variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftRightZeroFill :: Typed.TypedTerm t0
assignmentOperatorShiftRightZeroFill =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the simple variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorSimple :: Typed.TypedTerm t0
assignmentOperatorSimple =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the times variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorTimes :: Typed.TypedTerm t0
assignmentOperatorTimes =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the xor variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorXor :: Typed.TypedTerm t0
assignmentOperatorXor =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the expression field of hydra.java.syntax.Assignment
assignmentWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
assignmentWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the lhs field of hydra.java.syntax.Assignment
assignmentWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
assignmentWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.java.syntax.Assignment
assignmentWithOp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
assignmentWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.BasicForStatement
basicForStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
basicForStatement cond body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.BasicForStatement
basicForStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
basicForStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.BasicForStatement
basicForStatementCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
basicForStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
basicForStatementNoShortIf cond body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
basicForStatementNoShortIfBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
basicForStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
basicForStatementNoShortIfWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
basicForStatementNoShortIfWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.BasicForStatement
basicForStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
basicForStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.BasicForStatement
basicForStatementWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
basicForStatementWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.Block wrapper
block :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
block x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Block"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the localClassOrInterface variant of hydra.java.syntax.BlockStatement
blockStatementLocalClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
blockStatementLocalClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localClassOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the localVariableDeclaration variant of hydra.java.syntax.BlockStatement
blockStatementLocalVariableDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
blockStatementLocalVariableDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariableDeclaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the statement variant of hydra.java.syntax.BlockStatement
blockStatementStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
blockStatementStatement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the array variant of hydra.java.syntax.BooleanArray
booleanArrayArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
booleanArrayArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.BooleanArray
booleanArraySimple :: Typed.TypedTerm t0
booleanArraySimple =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.BreakStatement wrapper
breakStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
breakStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.BreakStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.CaseConstant wrapper
caseConstant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
caseConstant x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.CaseConstant"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.CasePattern
casePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
casePattern pattern guard =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Typed.unTypedTerm guard)}]}))
-- | DSL accessor for the guard field of hydra.java.syntax.CasePattern
casePatternGuard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
casePatternGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
        Core.projectionFieldName = (Core.Name "guard")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.java.syntax.CasePattern
casePatternPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
casePatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the guard field of hydra.java.syntax.CasePattern
casePatternWithGuard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
casePatternWithGuard original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.java.syntax.CasePattern
casePatternWithPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
casePatternWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
              Core.projectionFieldName = (Core.Name "guard")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the lambda variant of hydra.java.syntax.CastExpression
castExpressionLambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notPlusMinus variant of hydra.java.syntax.CastExpression
castExpressionNotPlusMinus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpressionNotPlusMinus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notPlusMinus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.CastExpression
castExpressionPrimitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpressionPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.CastExpression_Lambda
castExpression_Lambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_Lambda refAndBounds expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Typed.unTypedTerm refAndBounds)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_LambdaExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the refAndBounds field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaRefAndBounds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_LambdaRefAndBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
        Core.projectionFieldName = (Core.Name "refAndBounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_LambdaWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
              Core.projectionFieldName = (Core.Name "refAndBounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the refAndBounds field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaWithRefAndBounds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_LambdaWithRefAndBounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_NotPlusMinus refAndBounds expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Typed.unTypedTerm refAndBounds)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_NotPlusMinusExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the refAndBounds field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusRefAndBounds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_NotPlusMinusRefAndBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionFieldName = (Core.Name "refAndBounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_NotPlusMinusWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
              Core.projectionFieldName = (Core.Name "refAndBounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the refAndBounds field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithRefAndBounds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_NotPlusMinusWithRefAndBounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CastExpression_Primitive
castExpression_Primitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_Primitive type_ expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_PrimitiveExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_PrimitiveType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_PrimitiveWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_PrimitiveWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBounds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_RefAndBounds type_ bounds =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Typed.unTypedTerm bounds)}]}))
-- | DSL accessor for the bounds field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsBounds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_RefAndBoundsBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionFieldName = (Core.Name "bounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
castExpression_RefAndBoundsType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bounds field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithBounds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_RefAndBoundsWithBounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
castExpression_RefAndBoundsWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
              Core.projectionFieldName = (Core.Name "bounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CatchClause
catchClause :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchClause parameter block =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.CatchClause
catchClauseBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catchClauseBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameter field of hydra.java.syntax.CatchClause
catchClauseParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catchClauseParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.CatchClause
catchClauseWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchClauseWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the parameter field of hydra.java.syntax.CatchClause
catchClauseWithParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchClauseWithParameter original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CatchFormalParameter
catchFormalParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
catchFormalParameter modifiers type_ id =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catchFormalParameterId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catchFormalParameterModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catchFormalParameterType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterWithId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchFormalParameterWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchFormalParameterWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchFormalParameterWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CatchType
catchType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchType type_ types =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm types)}]}))
-- | DSL accessor for the type field of hydra.java.syntax.CatchType
catchTypeType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catchTypeType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the types field of hydra.java.syntax.CatchType
catchTypeTypes :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catchTypeTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the type field of hydra.java.syntax.CatchType
catchTypeWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchTypeWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the types field of hydra.java.syntax.CatchType
catchTypeWithTypes :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
catchTypeWithTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.Catches wrapper
catches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
catches x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Catches"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.ClassBody wrapper
classBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ClassBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the classMember variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationClassMember :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classBodyDeclarationClassMember x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classMember"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constructorDeclaration variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationConstructorDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classBodyDeclarationConstructorDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructorDeclaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the instanceInitializer variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationInstanceInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classBodyDeclarationInstanceInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the staticInitializer variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationStaticInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classBodyDeclarationStaticInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classBodyDeclarationWithComments value comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classBodyDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classBodyDeclarationWithCommentsValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classBodyDeclarationWithCommentsWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classBodyDeclarationWithCommentsWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the enum variant of hydra.java.syntax.ClassDeclaration
classDeclarationEnum :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDeclarationEnum x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.java.syntax.ClassDeclaration
classDeclarationNormal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDeclarationNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.java.syntax.ClassDeclaration
classDeclarationRecord :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDeclarationRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classInstanceCreationExpression qualifier expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classInstanceCreationExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classInstanceCreationExpressionQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classInstanceCreationExpressionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "qualifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classInstanceCreationExpressionWithQualifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the expression variant of hydra.java.syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classInstanceCreationExpression_QualifierExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classInstanceCreationExpression_QualifierPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.ClassLiteral
classLiteralBoolean :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classLiteralBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the numericType variant of hydra.java.syntax.ClassLiteral
classLiteralNumericType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classLiteralNumericType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numericType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.ClassLiteral
classLiteralType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classLiteralType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the void variant of hydra.java.syntax.ClassLiteral
classLiteralVoid :: Typed.TypedTerm t0
classLiteralVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the class variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classMemberDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the field variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationField :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classMemberDeclarationField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "field"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classMemberDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the method variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationMethod :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classMemberDeclarationMethod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationNone :: Typed.TypedTerm t0
classMemberDeclarationNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.ClassModifier
classModifierAbstract :: Typed.TypedTerm t0
classModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ClassModifier
classModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.ClassModifier
classModifierFinal :: Typed.TypedTerm t0
classModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nonSealed variant of hydra.java.syntax.ClassModifier
classModifierNonSealed :: Typed.TypedTerm t0
classModifierNonSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonSealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.ClassModifier
classModifierPrivate :: Typed.TypedTerm t0
classModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.ClassModifier
classModifierProtected :: Typed.TypedTerm t0
classModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ClassModifier
classModifierPublic :: Typed.TypedTerm t0
classModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sealed variant of hydra.java.syntax.ClassModifier
classModifierSealed :: Typed.TypedTerm t0
classModifierSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.ClassModifier
classModifierStatic :: Typed.TypedTerm t0
classModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.ClassModifier
classModifierStrictfp :: Typed.TypedTerm t0
classModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the class variant of hydra.java.syntax.ClassOrInterfaceType
classOrInterfaceTypeClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classOrInterfaceTypeClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.ClassOrInterfaceType
classOrInterfaceTypeInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classOrInterfaceTypeInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classOrInterfaceTypeToInstantiate identifiers typeArguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Typed.unTypedTerm identifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)}]}))
-- | DSL accessor for the identifiers field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateIdentifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classOrInterfaceTypeToInstantiateIdentifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionFieldName = (Core.Name "identifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classOrInterfaceTypeToInstantiateTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifiers field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithIdentifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classOrInterfaceTypeToInstantiateWithIdentifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classOrInterfaceTypeToInstantiateWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
              Core.projectionFieldName = (Core.Name "identifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ClassType
classType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
classType annotations qualifier identifier arguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.ClassType
classTypeAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classTypeAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the arguments field of hydra.java.syntax.ClassType
classTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ClassType
classTypeIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classTypeIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ClassType
classTypeQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classTypeQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the none variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierNone :: Typed.TypedTerm t0
classTypeQualifierNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the package variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierPackage :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classTypeQualifierPackage x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "package"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parent variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierParent :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classTypeQualifierParent x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parent"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the annotations field of hydra.java.syntax.ClassType
classTypeWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classTypeWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "qualifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the arguments field of hydra.java.syntax.ClassType
classTypeWithArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classTypeWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "qualifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.ClassType
classTypeWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classTypeWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "qualifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.ClassType
classTypeWithQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
classTypeWithQualifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
compactConstructorDeclaration modifiers name body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compactConstructorDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compactConstructorDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compactConstructorDeclarationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
compactConstructorDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
compactConstructorDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
compactConstructorDeclarationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the modular variant of hydra.java.syntax.CompilationUnit
compilationUnitModular :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compilationUnitModular x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modular"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ordinary variant of hydra.java.syntax.CompilationUnit
compilationUnitOrdinary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compilationUnitOrdinary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ConditionalAndExpression wrapper
conditionalAndExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalAndExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConditionalAndExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ternaryCond variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionTernaryCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpressionTernaryCond x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryCond"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ternaryLambda variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionTernaryLambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpressionTernaryLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryLambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
conditionalExpression_TernaryCond cond ifTrue ifFalse =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Typed.unTypedTerm ifTrue)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Typed.unTypedTerm ifFalse)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpression_TernaryCondCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondIfFalse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpression_TernaryCondIfFalse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionFieldName = (Core.Name "ifFalse")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondIfTrue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpression_TernaryCondIfTrue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionFieldName = (Core.Name "ifTrue")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
conditionalExpression_TernaryCondWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionFieldName = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionFieldName = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfFalse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
conditionalExpression_TernaryCondWithIfFalse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionFieldName = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfTrue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
conditionalExpression_TernaryCondWithIfTrue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionFieldName = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
conditionalExpression_TernaryLambda cond ifTrue ifFalse =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Typed.unTypedTerm ifTrue)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Typed.unTypedTerm ifFalse)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpression_TernaryLambdaCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaIfFalse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpression_TernaryLambdaIfFalse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionFieldName = (Core.Name "ifFalse")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaIfTrue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalExpression_TernaryLambdaIfTrue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionFieldName = (Core.Name "ifTrue")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
conditionalExpression_TernaryLambdaWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionFieldName = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionFieldName = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfFalse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
conditionalExpression_TernaryLambdaWithIfFalse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionFieldName = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfTrue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
conditionalExpression_TernaryLambdaWithIfTrue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionFieldName = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.ConditionalOrExpression wrapper
conditionalOrExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalOrExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConditionalOrExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.ConstantDeclaration
constantDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
constantDeclaration modifiers type_ variables =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm variables)}]}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.ConstantDeclaration
constantDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constantDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ConstantDeclaration
constantDeclarationType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constantDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variables field of hydra.java.syntax.ConstantDeclaration
constantDeclarationVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constantDeclarationVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ConstantDeclaration
constantDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constantDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.ConstantDeclaration
constantDeclarationWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constantDeclarationWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variables field of hydra.java.syntax.ConstantDeclaration
constantDeclarationWithVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constantDeclarationWithVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.ConstantExpression wrapper
constantExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constantExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConstantExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ConstantModifier
constantModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constantModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.ConstantModifier
constantModifierFinal :: Typed.TypedTerm t0
constantModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ConstantModifier
constantModifierPublic :: Typed.TypedTerm t0
constantModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.ConstantModifier
constantModifierStatic :: Typed.TypedTerm t0
constantModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.ConstructorBody
constructorBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorBody invocation statements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Typed.unTypedTerm invocation)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm statements)}]}))
-- | DSL accessor for the invocation field of hydra.java.syntax.ConstructorBody
constructorBodyInvocation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorBodyInvocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
        Core.projectionFieldName = (Core.Name "invocation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statements field of hydra.java.syntax.ConstructorBody
constructorBodyStatements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorBodyStatements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the invocation field of hydra.java.syntax.ConstructorBody
constructorBodyWithInvocation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorBodyWithInvocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.java.syntax.ConstructorBody
constructorBodyWithStatements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorBodyWithStatements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
              Core.projectionFieldName = (Core.Name "invocation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ConstructorDeclaration
constructorDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
constructorDeclaration modifiers constructor throws body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Typed.unTypedTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Typed.unTypedTerm throws)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constructor field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationConstructor :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclarationConstructor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "constructor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the throws field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationThrows :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclarationThrows x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "throws")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "constructor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "throws")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the constructor field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithConstructor :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclarationWithConstructor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "throws")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "constructor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "throws")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the throws field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithThrows :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclarationWithThrows original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "constructor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ConstructorDeclarator
constructorDeclarator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
constructorDeclarator parameters name receiverParameter formalParameters =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Typed.unTypedTerm receiverParameter)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Typed.unTypedTerm formalParameters)}]}))
-- | DSL accessor for the formalParameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorFormalParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclaratorFormalParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "formalParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclaratorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclaratorParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the receiverParameter field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorReceiverParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorDeclaratorReceiverParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "receiverParameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the formalParameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithFormalParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclaratorWithFormalParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclaratorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclaratorWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the receiverParameter field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithReceiverParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
constructorDeclaratorWithReceiverParameter original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionFieldName = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ConstructorModifier
constructorModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
constructorModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the private variant of hydra.java.syntax.ConstructorModifier
constructorModifierPrivate :: Typed.TypedTerm t0
constructorModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.ConstructorModifier
constructorModifierProtected :: Typed.TypedTerm t0
constructorModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ConstructorModifier
constructorModifierPublic :: Typed.TypedTerm t0
constructorModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.ContinueStatement wrapper
continueStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
continueStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ContinueStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.DefaultValue wrapper
defaultValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
defaultValue x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.DefaultValue"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.DimExpr
dimExpr :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
dimExpr annotations expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.DimExpr
dimExprAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dimExprAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.DimExpr
dimExprExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dimExprExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.DimExpr
dimExprWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
dimExprWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expression field of hydra.java.syntax.DimExpr
dimExprWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
dimExprWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.Dims wrapper
dims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dims x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Dims"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.DoStatement
doStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
doStatement body cond =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.DoStatement
doStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
doStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.DoStatement
doStatementCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
doStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.DoStatement
doStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
doStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the cond field of hydra.java.syntax.DoStatement
doStatementWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
doStatementWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ElementValue
elementValueAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
elementValueAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ElementValueArrayInitializer wrapper
elementValueArrayInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
elementValueArrayInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ElementValueArrayInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the conditionalExpression variant of hydra.java.syntax.ElementValue
elementValueConditionalExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
elementValueConditionalExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditionalExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the elementValueArrayInitializer variant of hydra.java.syntax.ElementValue
elementValueElementValueArrayInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
elementValueElementValueArrayInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elementValueArrayInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ElementValuePair
elementValuePair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
elementValuePair key value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the key field of hydra.java.syntax.ElementValuePair
elementValuePairKey :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
elementValuePairKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.ElementValuePair
elementValuePairValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
elementValuePairValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.java.syntax.ElementValuePair
elementValuePairWithKey :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
elementValuePairWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.java.syntax.ElementValuePair
elementValuePairWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
elementValuePairWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.EnhancedForCond
enhancedForCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForCond declaration expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Typed.unTypedTerm declaration)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the declaration field of hydra.java.syntax.EnhancedForCond
enhancedForCondDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enhancedForCondDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
        Core.projectionFieldName = (Core.Name "declaration")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.EnhancedForCond
enhancedForCondExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enhancedForCondExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declaration field of hydra.java.syntax.EnhancedForCond
enhancedForCondWithDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForCondWithDeclaration original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expression field of hydra.java.syntax.EnhancedForCond
enhancedForCondWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForCondWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
              Core.projectionFieldName = (Core.Name "declaration")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.EnhancedForStatement
enhancedForStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForStatement cond body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enhancedForStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enhancedForStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForStatementNoShortIf cond body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enhancedForStatementNoShortIfBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enhancedForStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForStatementNoShortIfWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForStatementNoShortIfWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enhancedForStatementWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.EnumBody wrapper
enumBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.EnumBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.EnumBody_Element
enumBody_Element :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumBody_Element constants bodyDeclarations =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Typed.unTypedTerm constants)},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Typed.unTypedTerm bodyDeclarations)}]}))
-- | DSL accessor for the bodyDeclarations field of hydra.java.syntax.EnumBody_Element
enumBody_ElementBodyDeclarations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumBody_ElementBodyDeclarations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
        Core.projectionFieldName = (Core.Name "bodyDeclarations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constants field of hydra.java.syntax.EnumBody_Element
enumBody_ElementConstants :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumBody_ElementConstants x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
        Core.projectionFieldName = (Core.Name "constants")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bodyDeclarations field of hydra.java.syntax.EnumBody_Element
enumBody_ElementWithBodyDeclarations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumBody_ElementWithBodyDeclarations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
              Core.projectionFieldName = (Core.Name "constants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the constants field of hydra.java.syntax.EnumBody_Element
enumBody_ElementWithConstants :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumBody_ElementWithConstants original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
              Core.projectionFieldName = (Core.Name "bodyDeclarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.EnumConstant
enumConstant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
enumConstant modifiers identifier arguments body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.EnumConstant
enumConstantArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumConstantArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.EnumConstant
enumConstantBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumConstantBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.EnumConstant
enumConstantIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumConstantIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.EnumConstantModifier wrapper
enumConstantModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumConstantModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.EnumConstantModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.EnumConstant
enumConstantModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumConstantModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.EnumConstant
enumConstantWithArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumConstantWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.EnumConstant
enumConstantWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumConstantWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.EnumConstant
enumConstantWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumConstantWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.EnumConstant
enumConstantWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumConstantWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.EnumDeclaration
enumDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
enumDeclaration modifiers identifier implements body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Typed.unTypedTerm implements)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.EnumDeclaration
enumDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.EnumDeclaration
enumDeclarationIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.EnumDeclaration
enumDeclarationImplements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumDeclarationImplements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "implements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.EnumDeclaration
enumDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
enumDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumDeclarationWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the implements field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithImplements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumDeclarationWithImplements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
enumDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the equal variant of hydra.java.syntax.EqualityExpression
equalityExpressionEqual :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
equalityExpressionEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notEqual variant of hydra.java.syntax.EqualityExpression
equalityExpressionNotEqual :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
equalityExpressionNotEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.EqualityExpression
equalityExpressionUnary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
equalityExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.EqualityExpression_Binary
equalityExpression_Binary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
equalityExpression_Binary lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
equalityExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
equalityExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
equalityExpression_BinaryWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
equalityExpression_BinaryWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the class variant of hydra.java.syntax.ExceptionType
exceptionTypeClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptionTypeClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ExceptionType
exceptionTypeVariable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptionTypeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ExclusiveOrExpression wrapper
exclusiveOrExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exclusiveOrExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ExclusiveOrExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
explicitConstructorInvocation typeArguments arguments variant =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm variant)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
explicitConstructorInvocationArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
explicitConstructorInvocationTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
explicitConstructorInvocationVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
explicitConstructorInvocationWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
explicitConstructorInvocationWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
explicitConstructorInvocationWithVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the primary variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
explicitConstructorInvocation_VariantPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantSuper :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
explicitConstructorInvocation_VariantSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the this variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantThis :: Typed.TypedTerm t0
explicitConstructorInvocation_VariantThis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the assignment variant of hydra.java.syntax.Expression
expressionAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.java.syntax.Expression
expressionLambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ExpressionName
expressionName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
expressionName qualifier identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ExpressionName
expressionNameIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionNameIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ExpressionName
expressionNameQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionNameQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.ExpressionName
expressionNameWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
expressionNameWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
              Core.projectionFieldName = (Core.Name "qualifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.ExpressionName
expressionNameWithQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
expressionNameWithQualifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.ExpressionStatement wrapper
expressionStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ExpressionStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.FieldAccess
fieldAccess :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
fieldAccess qualifier identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.FieldAccess
fieldAccessIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldAccessIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.FieldAccess
fieldAccessQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldAccessQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.FieldAccess
fieldAccessWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
fieldAccessWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "qualifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.FieldAccess
fieldAccessWithQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
fieldAccessWithQualifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the primary variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldAccess_QualifierPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierSuper :: Typed.TypedTerm t0
fieldAccess_QualifierSuper =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typed variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierTyped :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldAccess_QualifierTyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.FieldDeclaration
fieldDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
fieldDeclaration modifiers unannType variableDeclarators =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Typed.unTypedTerm unannType)},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Typed.unTypedTerm variableDeclarators)}]}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.FieldDeclaration
fieldDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the unannType field of hydra.java.syntax.FieldDeclaration
fieldDeclarationUnannType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldDeclarationUnannType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionFieldName = (Core.Name "unannType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variableDeclarators field of hydra.java.syntax.FieldDeclaration
fieldDeclarationVariableDeclarators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldDeclarationVariableDeclarators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionFieldName = (Core.Name "variableDeclarators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.FieldDeclaration
fieldDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
fieldDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionFieldName = (Core.Name "unannType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionFieldName = (Core.Name "variableDeclarators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the unannType field of hydra.java.syntax.FieldDeclaration
fieldDeclarationWithUnannType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
fieldDeclarationWithUnannType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionFieldName = (Core.Name "variableDeclarators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variableDeclarators field of hydra.java.syntax.FieldDeclaration
fieldDeclarationWithVariableDeclarators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
fieldDeclarationWithVariableDeclarators original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionFieldName = (Core.Name "unannType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the annotation variant of hydra.java.syntax.FieldModifier
fieldModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
fieldModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.FieldModifier
fieldModifierFinal :: Typed.TypedTerm t0
fieldModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.FieldModifier
fieldModifierPrivate :: Typed.TypedTerm t0
fieldModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.FieldModifier
fieldModifierProtected :: Typed.TypedTerm t0
fieldModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.FieldModifier
fieldModifierPublic :: Typed.TypedTerm t0
fieldModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.FieldModifier
fieldModifierStatic :: Typed.TypedTerm t0
fieldModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transient variant of hydra.java.syntax.FieldModifier
fieldModifierTransient :: Typed.TypedTerm t0
fieldModifierTransient =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transient"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the volatile variant of hydra.java.syntax.FieldModifier
fieldModifierVolatile :: Typed.TypedTerm t0
fieldModifierVolatile =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "volatile"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.Finally wrapper
finally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
finally x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Finally"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.FloatingPointLiteral wrapper
floatingPointLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
floatingPointLiteral x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.FloatingPointLiteral"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the double variant of hydra.java.syntax.FloatingPointType
floatingPointTypeDouble :: Typed.TypedTerm t0
floatingPointTypeDouble =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float variant of hydra.java.syntax.FloatingPointType
floatingPointTypeFloat :: Typed.TypedTerm t0
floatingPointTypeFloat =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.ForCond
forCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
forCond init cond update =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Typed.unTypedTerm update)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.ForCond
forCondCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forCondCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.java.syntax.ForCond
forCondInit :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forCondInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the update field of hydra.java.syntax.ForCond
forCondUpdate :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forCondUpdate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionFieldName = (Core.Name "update")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ForCond
forCondWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
forCondWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionFieldName = (Core.Name "update")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the init field of hydra.java.syntax.ForCond
forCondWithInit :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
forCondWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionFieldName = (Core.Name "update")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the update field of hydra.java.syntax.ForCond
forCondWithUpdate :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
forCondWithUpdate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the localVariable variant of hydra.java.syntax.ForInit
forInitLocalVariable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forInitLocalVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the statements variant of hydra.java.syntax.ForInit
forInitStatements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forInitStatements x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statements"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the basic variant of hydra.java.syntax.ForStatement
forStatementBasic :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementBasic x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the enhanced variant of hydra.java.syntax.ForStatement
forStatementEnhanced :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementEnhanced x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the basic variant of hydra.java.syntax.ForStatementNoShortIf
forStatementNoShortIfBasic :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementNoShortIfBasic x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the enhanced variant of hydra.java.syntax.ForStatementNoShortIf
forStatementNoShortIfEnhanced :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementNoShortIfEnhanced x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ForUpdate wrapper
forUpdate :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forUpdate x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ForUpdate"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.FormalParameter
formalParameterSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
formalParameterSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.FormalParameter
formalParameterVariableArity :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
formalParameterVariableArity x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.FormalParameter_Simple
formalParameter_Simple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
formalParameter_Simple modifiers type_ id =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
formalParameter_SimpleId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
formalParameter_SimpleModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
formalParameter_SimpleType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleWithId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
formalParameter_SimpleWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
formalParameter_SimpleWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
formalParameter_SimpleWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.Guard wrapper
guard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
guard x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Guard"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.Identifier wrapper
identifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
identifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Identifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.IfThenElseStatement
ifThenElseStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
ifThenElseStatement cond then_ else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenElseStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenElseStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
ifThenElseStatementNoShortIf cond then_ else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenElseStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenElseStatementNoShortIfElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the then field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfThen :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenElseStatementNoShortIfThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenElseStatementNoShortIfWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenElseStatementNoShortIfWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the then field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithThen :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenElseStatementNoShortIfWithThen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the then field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementThen :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenElseStatementThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenElseStatementWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementWithElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenElseStatementWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the then field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementWithThen :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenElseStatementWithThen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.IfThenStatement
ifThenStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenStatement expression statement =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm statement)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.IfThenStatement
ifThenStatementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenStatementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statement field of hydra.java.syntax.IfThenStatement
ifThenStatementStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifThenStatementStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.IfThenStatement
ifThenStatementWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenStatementWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
              Core.projectionFieldName = (Core.Name "statement")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.java.syntax.IfThenStatement
ifThenStatementWithStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ifThenStatementWithStatement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the singleStaticImport variant of hydra.java.syntax.ImportDeclaration
importDeclarationSingleStaticImport :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importDeclarationSingleStaticImport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleStaticImport"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the singleType variant of hydra.java.syntax.ImportDeclaration
importDeclarationSingleType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importDeclarationSingleType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the staticImportOnDemand variant of hydra.java.syntax.ImportDeclaration
importDeclarationStaticImportOnDemand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importDeclarationStaticImportOnDemand x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticImportOnDemand"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeImportOnDemand variant of hydra.java.syntax.ImportDeclaration
importDeclarationTypeImportOnDemand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importDeclarationTypeImportOnDemand x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeImportOnDemand"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.InclusiveOrExpression wrapper
inclusiveOrExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
inclusiveOrExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InclusiveOrExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.InstanceInitializer wrapper
instanceInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
instanceInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InstanceInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.InstanceofExpression
instanceofExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
instanceofExpression lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
instanceofExpressionLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
instanceofExpressionRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
instanceofExpressionWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
instanceofExpressionWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the pattern variant of hydra.java.syntax.InstanceofExpression_Rhs
instanceofExpression_RhsPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
instanceofExpression_RhsPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression_Rhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the referenceType variant of hydra.java.syntax.InstanceofExpression_Rhs
instanceofExpression_RhsReferenceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
instanceofExpression_RhsReferenceType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression_Rhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "referenceType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.IntegerLiteral wrapper
integerLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
integerLiteral x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.IntegerLiteral"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the byte variant of hydra.java.syntax.IntegralType
integralTypeByte :: Typed.TypedTerm t0
integralTypeByte =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the char variant of hydra.java.syntax.IntegralType
integralTypeChar :: Typed.TypedTerm t0
integralTypeChar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int variant of hydra.java.syntax.IntegralType
integralTypeInt :: Typed.TypedTerm t0
integralTypeInt =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the long variant of hydra.java.syntax.IntegralType
integralTypeLong :: Typed.TypedTerm t0
integralTypeLong =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the short variant of hydra.java.syntax.IntegralType
integralTypeShort :: Typed.TypedTerm t0
integralTypeShort =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.InterfaceBody wrapper
interfaceBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InterfaceBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotationInterface variant of hydra.java.syntax.InterfaceDeclaration
interfaceDeclarationAnnotationInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceDeclarationAnnotationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normalInterface variant of hydra.java.syntax.InterfaceDeclaration
interfaceDeclarationNormalInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceDeclarationNormalInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMemberDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constant variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationConstant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMemberDeclarationConstant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMemberDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interfaceMethod variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterfaceMethod :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMemberDeclarationInterfaceMethod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interfaceMethod"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
interfaceMemberDeclarationWithComments value comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMemberDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMemberDeclarationWithCommentsValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsWithComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
interfaceMemberDeclarationWithCommentsWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
interfaceMemberDeclarationWithCommentsWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
interfaceMethodDeclaration modifiers header body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMethodDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMethodDeclarationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMethodDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
interfaceMethodDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
interfaceMethodDeclarationWithHeader original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
interfaceMethodDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the abstract variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierAbstract :: Typed.TypedTerm t0
interfaceMethodModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceMethodModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the default variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierDefault :: Typed.TypedTerm t0
interfaceMethodModifierDefault =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierPrivate :: Typed.TypedTerm t0
interfaceMethodModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierPublic :: Typed.TypedTerm t0
interfaceMethodModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierStatic :: Typed.TypedTerm t0
interfaceMethodModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierStrictfp :: Typed.TypedTerm t0
interfaceMethodModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.InterfaceModifier
interfaceModifierAbstract :: Typed.TypedTerm t0
interfaceModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.InterfaceModifier
interfaceModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nonSealed variant of hydra.java.syntax.InterfaceModifier
interfaceModifierNonSealed :: Typed.TypedTerm t0
interfaceModifierNonSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonSealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.InterfaceModifier
interfaceModifierPrivate :: Typed.TypedTerm t0
interfaceModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.InterfaceModifier
interfaceModifierProtected :: Typed.TypedTerm t0
interfaceModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.InterfaceModifier
interfaceModifierPublic :: Typed.TypedTerm t0
interfaceModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sealed variant of hydra.java.syntax.InterfaceModifier
interfaceModifierSealed :: Typed.TypedTerm t0
interfaceModifierSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.InterfaceModifier
interfaceModifierStatic :: Typed.TypedTerm t0
interfaceModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.InterfaceModifier
interfaceModifierStrictfp :: Typed.TypedTerm t0
interfaceModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.InterfaceType wrapper
interfaceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interfaceType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InterfaceType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.LabeledStatement
labeledStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
labeledStatement identifier statement =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm statement)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.LabeledStatement
labeledStatementIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
labeledStatementIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
labeledStatementNoShortIf identifier statement =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm statement)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
labeledStatementNoShortIfIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statement field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
labeledStatementNoShortIfStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
labeledStatementNoShortIfWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "statement")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
labeledStatementNoShortIfWithStatement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the statement field of hydra.java.syntax.LabeledStatement
labeledStatementStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
labeledStatementStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.LabeledStatement
labeledStatementWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
labeledStatementWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
              Core.projectionFieldName = (Core.Name "statement")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.java.syntax.LabeledStatement
labeledStatementWithStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
labeledStatementWithStatement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the block variant of hydra.java.syntax.LambdaBody
lambdaBodyBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaBodyBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.LambdaBody
lambdaBodyExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaBodyExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LambdaExpression
lambdaExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
lambdaExpression parameters body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.LambdaExpression
lambdaExpressionBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.LambdaExpression
lambdaExpressionParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaExpressionParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.LambdaExpression
lambdaExpressionWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
lambdaExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.LambdaExpression
lambdaExpressionWithParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
lambdaExpressionWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the normal variant of hydra.java.syntax.LambdaParameter
lambdaParameterNormal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParameterNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.LambdaParameterType
lambdaParameterTypeType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParameterTypeType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.java.syntax.LambdaParameterType
lambdaParameterTypeVar :: Typed.TypedTerm t0
lambdaParameterTypeVar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.LambdaParameter
lambdaParameterVariableArity :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParameterVariableArity x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_Normal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
lambdaParameter_Normal modifiers type_ id =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParameter_NormalId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParameter_NormalModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParameter_NormalType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalWithId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
lambdaParameter_NormalWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
lambdaParameter_NormalWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
lambdaParameter_NormalWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the single variant of hydra.java.syntax.LambdaParameters
lambdaParametersSingle :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParametersSingle x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.java.syntax.LambdaParameters
lambdaParametersTuple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParametersTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the arrayAccess variant of hydra.java.syntax.LeftHandSide
leftHandSideArrayAccess :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
leftHandSideArrayAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expressionName variant of hydra.java.syntax.LeftHandSide
leftHandSideExpressionName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
leftHandSideExpressionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.LeftHandSide
leftHandSideFieldAccess :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
leftHandSideFieldAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.Literal
literalBoolean :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the character variant of hydra.java.syntax.Literal
literalCharacter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalCharacter x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the floatingPoint variant of hydra.java.syntax.Literal
literalFloatingPoint :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalFloatingPoint x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.java.syntax.Literal
literalInteger :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the null variant of hydra.java.syntax.Literal
literalNull :: Typed.TypedTerm t0
literalNull =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the string variant of hydra.java.syntax.Literal
literalString :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the textBlock variant of hydra.java.syntax.Literal
literalTextBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalTextBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "textBlock"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
localClassOrInterfaceDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normalInterface variant of hydra.java.syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationNormalInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
localClassOrInterfaceDeclarationNormalInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LocalVariableDeclaration
localVariableDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
localVariableDeclaration modifiers type_ declarators =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Typed.unTypedTerm declarators)}]}))
-- | DSL accessor for the declarators field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationDeclarators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
localVariableDeclarationDeclarators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionFieldName = (Core.Name "declarators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
localVariableDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.LocalVariableDeclarationStatement wrapper
localVariableDeclarationStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
localVariableDeclarationStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclarationStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
localVariableDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarators field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationWithDeclarators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
localVariableDeclarationWithDeclarators original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
localVariableDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionFieldName = (Core.Name "declarators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
localVariableDeclarationWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionFieldName = (Core.Name "declarators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the type variant of hydra.java.syntax.LocalVariableType
localVariableTypeType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
localVariableTypeType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.java.syntax.LocalVariableType
localVariableTypeVar :: Typed.TypedTerm t0
localVariableTypeVar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.MarkerAnnotation wrapper
markerAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
markerAnnotation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MarkerAnnotation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the block variant of hydra.java.syntax.MethodBody
methodBodyBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodBodyBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.MethodBody
methodBodyNone :: Typed.TypedTerm t0
methodBodyNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.MethodDeclaration
methodDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
methodDeclaration annotations modifiers header body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.MethodDeclaration
methodDeclarationAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodDeclarationAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.MethodDeclaration
methodDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.MethodDeclaration
methodDeclarationHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodDeclarationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.MethodDeclaration
methodDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodDeclarationWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodDeclarationWithHeader original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodDeclarator
methodDeclarator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
methodDeclarator identifier receiverParameter formalParameters =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Typed.unTypedTerm receiverParameter)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Typed.unTypedTerm formalParameters)}]}))
-- | DSL accessor for the formalParameters field of hydra.java.syntax.MethodDeclarator
methodDeclaratorFormalParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodDeclaratorFormalParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionFieldName = (Core.Name "formalParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodDeclarator
methodDeclaratorIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodDeclaratorIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the receiverParameter field of hydra.java.syntax.MethodDeclarator
methodDeclaratorReceiverParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodDeclaratorReceiverParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionFieldName = (Core.Name "receiverParameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the formalParameters field of hydra.java.syntax.MethodDeclarator
methodDeclaratorWithFormalParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodDeclaratorWithFormalParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionFieldName = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodDeclarator
methodDeclaratorWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodDeclaratorWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionFieldName = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionFieldName = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the receiverParameter field of hydra.java.syntax.MethodDeclarator
methodDeclaratorWithReceiverParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodDeclaratorWithReceiverParameter original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionFieldName = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodHeader
methodHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
methodHeader parameters result declarator throws =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Typed.unTypedTerm result)},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Typed.unTypedTerm declarator)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Typed.unTypedTerm throws)}]}))
-- | DSL accessor for the declarator field of hydra.java.syntax.MethodHeader
methodHeaderDeclarator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodHeaderDeclarator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "declarator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.MethodHeader
methodHeaderParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodHeaderParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the result field of hydra.java.syntax.MethodHeader
methodHeaderResult :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodHeaderResult x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "result")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the throws field of hydra.java.syntax.MethodHeader
methodHeaderThrows :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodHeaderThrows x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "throws")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarator field of hydra.java.syntax.MethodHeader
methodHeaderWithDeclarator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodHeaderWithDeclarator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "throws")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.MethodHeader
methodHeaderWithParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodHeaderWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "declarator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "throws")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the result field of hydra.java.syntax.MethodHeader
methodHeaderWithResult :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodHeaderWithResult original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "declarator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "throws")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the throws field of hydra.java.syntax.MethodHeader
methodHeaderWithThrows :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodHeaderWithThrows original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionFieldName = (Core.Name "declarator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.MethodInvocation
methodInvocation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodInvocation header arguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.MethodInvocation
methodInvocationArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocationArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.MethodInvocation
methodInvocationHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.MethodInvocation
methodInvocationWithArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodInvocationWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.MethodInvocation
methodInvocationWithHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodInvocationWithHeader original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodInvocation_Complex
methodInvocation_Complex :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
methodInvocation_Complex variant typeArguments identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm variant)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_ComplexIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_ComplexTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_ComplexVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodInvocation_ComplexWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodInvocation_ComplexWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexWithVariant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodInvocation_ComplexWithVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the complex variant of hydra.java.syntax.MethodInvocation_Header
methodInvocation_HeaderComplex :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_HeaderComplex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.MethodInvocation_Header
methodInvocation_HeaderSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_HeaderSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_VariantExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_VariantPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantSuper :: Typed.TypedTerm t0
methodInvocation_VariantSuper =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_VariantType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeSuper variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantTypeSuper :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodInvocation_VariantTypeSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSuper"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.MethodModifier
methodModifierAbstract :: Typed.TypedTerm t0
methodModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.MethodModifier
methodModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.MethodModifier
methodModifierFinal :: Typed.TypedTerm t0
methodModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the native variant of hydra.java.syntax.MethodModifier
methodModifierNative :: Typed.TypedTerm t0
methodModifierNative =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "native"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.MethodModifier
methodModifierPrivate :: Typed.TypedTerm t0
methodModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.MethodModifier
methodModifierProtected :: Typed.TypedTerm t0
methodModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.MethodModifier
methodModifierPublic :: Typed.TypedTerm t0
methodModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.MethodModifier
methodModifierStatic :: Typed.TypedTerm t0
methodModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.MethodModifier
methodModifierStrictfp :: Typed.TypedTerm t0
methodModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the synchronized variant of hydra.java.syntax.MethodModifier
methodModifierSynchronized :: Typed.TypedTerm t0
methodModifierSynchronized =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.MethodName wrapper
methodName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MethodName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the array variant of hydra.java.syntax.MethodReference
methodReferenceArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReferenceArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.MethodReference
methodReferenceExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReferenceExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the new variant of hydra.java.syntax.MethodReference
methodReferenceNew :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReferenceNew x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.MethodReference
methodReferencePrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReferencePrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the referenceType variant of hydra.java.syntax.MethodReference
methodReferenceReferenceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReferenceReferenceType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "referenceType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.MethodReference
methodReferenceSuper :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReferenceSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.MethodReference_Array wrapper
methodReference_Array :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_Array x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MethodReference_Array"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.MethodReference_Expression
methodReference_Expression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
methodReference_Expression name typeArguments identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_ExpressionIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_ExpressionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_ExpressionTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_ExpressionWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_ExpressionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_ExpressionWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_New
methodReference_New :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_New classType typeArguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Typed.unTypedTerm classType)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)}]}))
-- | DSL accessor for the classType field of hydra.java.syntax.MethodReference_New
methodReference_NewClassType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_NewClassType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
        Core.projectionFieldName = (Core.Name "classType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_New
methodReference_NewTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_NewTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the classType field of hydra.java.syntax.MethodReference_New
methodReference_NewWithClassType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_NewWithClassType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_New
methodReference_NewWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_NewWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
              Core.projectionFieldName = (Core.Name "classType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_Primary
methodReference_Primary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
methodReference_Primary primary typeArguments identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_PrimaryIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_PrimaryPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_PrimaryTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_PrimaryWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryWithPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_PrimaryWithPrimary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_PrimaryWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
methodReference_ReferenceType referenceType typeArguments identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Typed.unTypedTerm referenceType)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_ReferenceTypeIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the referenceType field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeReferenceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_ReferenceTypeReferenceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionFieldName = (Core.Name "referenceType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_ReferenceTypeTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_ReferenceTypeWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionFieldName = (Core.Name "referenceType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the referenceType field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithReferenceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_ReferenceTypeWithReferenceType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_ReferenceTypeWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionFieldName = (Core.Name "referenceType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_Super
methodReference_Super :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
methodReference_Super typeArguments identifier super =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Typed.unTypedTerm super)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_Super
methodReference_SuperIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_SuperIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the super field of hydra.java.syntax.MethodReference_Super
methodReference_SuperSuper :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_SuperSuper x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionFieldName = (Core.Name "super")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Super
methodReference_SuperTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
methodReference_SuperTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Super
methodReference_SuperWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_SuperWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionFieldName = (Core.Name "super")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the super field of hydra.java.syntax.MethodReference_Super
methodReference_SuperWithSuper :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_SuperWithSuper original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_Super
methodReference_SuperWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
methodReference_SuperWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionFieldName = (Core.Name "super")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ModularCompilationUnit
modularCompilationUnit :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
modularCompilationUnit imports module_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm module_)}]}))
-- | DSL accessor for the imports field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitImports :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
modularCompilationUnitImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the module field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitModule :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
modularCompilationUnitModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the imports field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitWithImports :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
modularCompilationUnitWithImports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the module field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitWithModule :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
modularCompilationUnitWithModule original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ModuleDeclaration
moduleDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
moduleDeclaration annotations open identifiers directives =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Typed.unTypedTerm open)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Typed.unTypedTerm identifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Typed.unTypedTerm directives)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDeclarationAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the directives field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationDirectives :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDeclarationDirectives x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "directives")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifiers field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationIdentifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDeclarationIdentifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "identifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the open field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationOpen :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDeclarationOpen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "open")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDeclarationWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "open")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "identifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "directives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the directives field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithDirectives :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDeclarationWithDirectives original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "open")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "identifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifiers field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithIdentifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDeclarationWithIdentifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "open")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "directives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the open field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithOpen :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDeclarationWithOpen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "identifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "directives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the exports variant of hydra.java.syntax.ModuleDirective
moduleDirectiveExports :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirectiveExports x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exports"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the opens variant of hydra.java.syntax.ModuleDirective
moduleDirectiveOpens :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirectiveOpens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the provides variant of hydra.java.syntax.ModuleDirective
moduleDirectiveProvides :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirectiveProvides x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "provides"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the requires variant of hydra.java.syntax.ModuleDirective
moduleDirectiveRequires :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirectiveRequires x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "requires"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the uses variant of hydra.java.syntax.ModuleDirective
moduleDirectiveUses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirectiveUses x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uses"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpens :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_ExportsOrOpens package modules =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Typed.unTypedTerm modules)}]}))
-- | DSL accessor for the modules field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensModules :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirective_ExportsOrOpensModules x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionFieldName = (Core.Name "modules")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensPackage :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirective_ExportsOrOpensPackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modules field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithModules :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_ExportsOrOpensWithModules original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the package field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithPackage :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_ExportsOrOpensWithPackage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ModuleDirective_Provides
moduleDirective_Provides :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_Provides to with =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Typed.unTypedTerm to)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Typed.unTypedTerm with)}]}))
-- | DSL accessor for the to field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesTo :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirective_ProvidesTo x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
        Core.projectionFieldName = (Core.Name "to")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the with field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWith :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirective_ProvidesWith x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the to field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithTo :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_ProvidesWithTo original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the with field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithWith :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_ProvidesWithWith original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
              Core.projectionFieldName = (Core.Name "to")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ModuleDirective_Requires
moduleDirective_Requires :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_Requires modifiers module_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm module_)}]}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirective_RequiresModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the module field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresModule :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleDirective_RequiresModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_RequiresWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the module field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModule :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleDirective_RequiresWithModule original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ModuleName
moduleName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleName identifier name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ModuleName
moduleNameIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleNameIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.ModuleName
moduleNameName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
moduleNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.ModuleName
moduleNameWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleNameWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.java.syntax.ModuleName
moduleNameWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
moduleNameWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the divide variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionDivide :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
multiplicativeExpressionDivide x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mod variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionMod :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
multiplicativeExpressionMod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the times variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionTimes :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
multiplicativeExpressionTimes x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionUnary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
multiplicativeExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_Binary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
multiplicativeExpression_Binary lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
multiplicativeExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
multiplicativeExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
multiplicativeExpression_BinaryWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
multiplicativeExpression_BinaryWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.NormalAnnotation
normalAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalAnnotation typeName pairs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Typed.unTypedTerm pairs)}]}))
-- | DSL accessor for the pairs field of hydra.java.syntax.NormalAnnotation
normalAnnotationPairs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalAnnotationPairs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
        Core.projectionFieldName = (Core.Name "pairs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.java.syntax.NormalAnnotation
normalAnnotationTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalAnnotationTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pairs field of hydra.java.syntax.NormalAnnotation
normalAnnotationWithPairs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalAnnotationWithPairs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.java.syntax.NormalAnnotation
normalAnnotationWithTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalAnnotationWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
              Core.projectionFieldName = (Core.Name "pairs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.NormalClassDeclaration
normalClassDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6 -> Typed.TypedTerm t7
normalClassDeclaration modifiers identifier parameters extends implements permits body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Typed.unTypedTerm extends)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Typed.unTypedTerm implements)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Typed.unTypedTerm permits)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalClassDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the extends field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationExtends :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalClassDeclarationExtends x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "extends")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalClassDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationImplements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalClassDeclarationImplements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "implements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalClassDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalClassDeclarationParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the permits field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationPermits :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalClassDeclarationPermits x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "permits")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalClassDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the extends field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithExtends :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalClassDeclarationWithExtends original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalClassDeclarationWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the implements field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithImplements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalClassDeclarationWithImplements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalClassDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalClassDeclarationWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the permits field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithPermits :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalClassDeclarationWithPermits original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6
normalInterfaceDeclaration modifiers identifier parameters extends permits body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Typed.unTypedTerm extends)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Typed.unTypedTerm permits)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalInterfaceDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the extends field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationExtends :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalInterfaceDeclarationExtends x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "extends")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalInterfaceDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalInterfaceDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalInterfaceDeclarationParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the permits field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationPermits :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
normalInterfaceDeclarationPermits x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "permits")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalInterfaceDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the extends field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithExtends :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalInterfaceDeclarationWithExtends original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalInterfaceDeclarationWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalInterfaceDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalInterfaceDeclarationWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "permits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the permits field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithPermits :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
normalInterfaceDeclarationWithPermits original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the array variant of hydra.java.syntax.NumericTypeArray
numericTypeArrayArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
numericTypeArrayArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.NumericTypeArray
numericTypeArraySimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
numericTypeArraySimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the floatingPoint variant of hydra.java.syntax.NumericType
numericTypeFloatingPoint :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
numericTypeFloatingPoint x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integral variant of hydra.java.syntax.NumericType
numericTypeIntegral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
numericTypeIntegral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integral"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnit :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
ordinaryCompilationUnit package imports types =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm types)}]}))
-- | DSL accessor for the imports field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitImports :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ordinaryCompilationUnitImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitPackage :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ordinaryCompilationUnitPackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the types field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitTypes :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ordinaryCompilationUnitTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the imports field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithImports :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ordinaryCompilationUnitWithImports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the package field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithPackage :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ordinaryCompilationUnitWithPackage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the types field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithTypes :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
ordinaryCompilationUnitWithTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.PackageDeclaration
packageDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
packageDeclaration modifiers identifiers =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Typed.unTypedTerm identifiers)}]}))
-- | DSL accessor for the identifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationIdentifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
packageDeclarationIdentifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
        Core.projectionFieldName = (Core.Name "identifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
packageDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationWithIdentifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
packageDeclarationWithIdentifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
packageDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
              Core.projectionFieldName = (Core.Name "identifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.PackageModifier wrapper
packageModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
packageModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PackageName wrapper
packageName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
packageName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PackageOrTypeName wrapper
packageOrTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
packageOrTypeName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageOrTypeName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the record variant of hydra.java.syntax.Pattern
patternRecord :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
patternRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.Pattern
patternType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
patternType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.PostDecrementExpression wrapper
postDecrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
postDecrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PostDecrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PostIncrementExpression wrapper
postIncrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
postIncrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PostIncrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the name variant of hydra.java.syntax.PostfixExpression
postfixExpressionName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
postfixExpressionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postDecrement variant of hydra.java.syntax.PostfixExpression
postfixExpressionPostDecrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
postfixExpressionPostDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postIncrement variant of hydra.java.syntax.PostfixExpression
postfixExpressionPostIncrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
postfixExpressionPostIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.PostfixExpression
postfixExpressionPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
postfixExpressionPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.PreDecrementExpression wrapper
preDecrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
preDecrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PreDecrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PreIncrementExpression wrapper
preIncrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
preIncrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PreIncrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the arrayCreation variant of hydra.java.syntax.Primary
primaryArrayCreation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryArrayCreation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayCreation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the noNewArray variant of hydra.java.syntax.Primary
primaryNoNewArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noNewArray"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the arrayAccess variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionArrayAccess :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionArrayAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classInstance variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassInstance :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionClassInstance x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstance"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classLiteral variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionClassLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classLiteral"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dotThis variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionDotThis :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionDotThis x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dotThis"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionFieldAccess :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionFieldAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the methodInvocation variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodInvocation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionMethodInvocation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the methodReference variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodReference :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionMethodReference x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodReference"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionParens :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryNoNewArrayExpressionParens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the this variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionThis :: Typed.TypedTerm t0
primaryNoNewArrayExpressionThis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.PrimitiveType
primitiveTypeBoolean :: Typed.TypedTerm t0
primitiveTypeBoolean =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the numeric variant of hydra.java.syntax.PrimitiveType
primitiveTypeNumeric :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primitiveTypeNumeric x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
primitiveTypeWithAnnotations type_ annotations =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primitiveTypeWithAnnotationsAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primitiveTypeWithAnnotationsType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
primitiveTypeWithAnnotationsWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
primitiveTypeWithAnnotationsWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ReceiverParameter
receiverParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
receiverParameter annotations unannType identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Typed.unTypedTerm unannType)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.ReceiverParameter
receiverParameterAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
receiverParameterAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ReceiverParameter
receiverParameterIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
receiverParameterIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the unannType field of hydra.java.syntax.ReceiverParameter
receiverParameterUnannType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
receiverParameterUnannType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionFieldName = (Core.Name "unannType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.ReceiverParameter
receiverParameterWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
receiverParameterWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionFieldName = (Core.Name "unannType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.ReceiverParameter
receiverParameterWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
receiverParameterWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionFieldName = (Core.Name "unannType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the unannType field of hydra.java.syntax.ReceiverParameter
receiverParameterWithUnannType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
receiverParameterWithUnannType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.RecordBody wrapper
recordBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the classBody variant of hydra.java.syntax.RecordBodyDeclaration
recordBodyDeclarationClassBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordBodyDeclarationClassBody x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classBody"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the compactConstructor variant of hydra.java.syntax.RecordBodyDeclaration
recordBodyDeclarationCompactConstructor :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordBodyDeclarationCompactConstructor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compactConstructor"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.RecordComponentModifier wrapper
recordComponentModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordComponentModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordComponentModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.RecordComponent
recordComponentSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordComponentSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.RecordComponent
recordComponentVariableArity :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordComponentVariableArity x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.RecordComponent_Simple
recordComponent_Simple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
recordComponent_Simple modifiers type_ identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordComponent_SimpleIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordComponent_SimpleModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordComponent_SimpleType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordComponent_SimpleWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordComponent_SimpleWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordComponent_SimpleWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.RecordDeclaration
recordDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6
recordDeclaration modifiers identifier parameters header implements body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Typed.unTypedTerm implements)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.RecordDeclaration
recordDeclarationBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.RecordDeclaration
recordDeclarationHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordDeclarationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.RecordDeclaration
recordDeclarationIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.RecordDeclaration
recordDeclarationImplements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordDeclarationImplements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "implements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.RecordDeclaration
recordDeclarationModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.RecordDeclaration
recordDeclarationParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordDeclarationParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordDeclarationWithHeader original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordDeclarationWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the implements field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithImplements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordDeclarationWithImplements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordDeclarationWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordDeclarationWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "implements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.RecordHeader wrapper
recordHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordHeader x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordHeader"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.RecordPattern
recordPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordPattern type_ patterns =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm patterns)}]}))
-- | DSL accessor for the patterns field of hydra.java.syntax.RecordPattern
recordPatternPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordPatternPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.RecordPattern
recordPatternType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
recordPatternType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the patterns field of hydra.java.syntax.RecordPattern
recordPatternWithPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordPatternWithPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.RecordPattern
recordPatternWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
recordPatternWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the array variant of hydra.java.syntax.ReferenceType
referenceTypeArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
referenceTypeArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ReferenceType
referenceTypeClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
referenceTypeClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ReferenceType
referenceTypeVariable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
referenceTypeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the greaterThan variant of hydra.java.syntax.RelationalExpression
relationalExpressionGreaterThan :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpressionGreaterThan x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the greaterThanEqual variant of hydra.java.syntax.RelationalExpression
relationalExpressionGreaterThanEqual :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpressionGreaterThanEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanEqual"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the instanceofExpression variant of hydra.java.syntax.RelationalExpression
relationalExpressionInstanceofExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpressionInstanceofExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceofExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lessThan variant of hydra.java.syntax.RelationalExpression
relationalExpressionLessThan :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpressionLessThan x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lessThanEqual variant of hydra.java.syntax.RelationalExpression
relationalExpressionLessThanEqual :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpressionLessThanEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanEqual"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.RelationalExpression
relationalExpressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThan :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_GreaterThan lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqual :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_GreaterThanEqual lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_GreaterThanEqualLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_GreaterThanEqualRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_GreaterThanEqualWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_GreaterThanEqualWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_GreaterThanLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_GreaterThanRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_GreaterThanWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_GreaterThanWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThan :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_LessThan lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqual :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_LessThanEqual lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_LessThanEqualLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_LessThanEqualRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_LessThanEqualWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_LessThanEqualWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_LessThanLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
relationalExpression_LessThanRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_LessThanWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
relationalExpression_LessThanWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the static variant of hydra.java.syntax.RequiresModifier
requiresModifierStatic :: Typed.TypedTerm t0
requiresModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transitive variant of hydra.java.syntax.RequiresModifier
requiresModifierTransitive :: Typed.TypedTerm t0
requiresModifierTransitive =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transitive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the local variant of hydra.java.syntax.Resource
resourceLocal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resourceLocal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ResourceSpecification wrapper
resourceSpecification :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resourceSpecification x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ResourceSpecification"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the variable variant of hydra.java.syntax.Resource
resourceVariable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resourceVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.Resource_Local
resource_Local :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
resource_Local modifiers type_ identifier expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.Resource_Local
resource_LocalExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resource_LocalExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.Resource_Local
resource_LocalIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resource_LocalIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.Resource_Local
resource_LocalModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resource_LocalModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.Resource_Local
resource_LocalType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resource_LocalType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.Resource_Local
resource_LocalWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
resource_LocalWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.Resource_Local
resource_LocalWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
resource_LocalWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.Resource_Local
resource_LocalWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
resource_LocalWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.Resource_Local
resource_LocalWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
resource_LocalWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the type variant of hydra.java.syntax.Result
resultType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
resultType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the void variant of hydra.java.syntax.Result
resultVoid :: Typed.TypedTerm t0
resultVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.ReturnStatement wrapper
returnStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
returnStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ReturnStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the shiftLeft variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftLeft :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpressionShiftLeft x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the shiftRight variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftRight :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpressionShiftRight x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the shiftRightZeroFill variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftRightZeroFill :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpressionShiftRightZeroFill x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.ShiftExpression
shiftExpressionUnary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ShiftExpression_Binary
shiftExpression_Binary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
shiftExpression_Binary lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
shiftExpression_BinaryWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
shiftExpression_BinaryWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.SimpleTypeName wrapper
simpleTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleTypeName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.SimpleTypeName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.SingleElementAnnotation
singleElementAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
singleElementAnnotation name value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the name field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleElementAnnotationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleElementAnnotationValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
singleElementAnnotationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
singleElementAnnotationWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
singleStaticImportDeclaration typeName identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleStaticImportDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleStaticImportDeclarationTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
singleStaticImportDeclarationWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
singleStaticImportDeclarationWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.SingleTypeImportDeclaration wrapper
singleTypeImportDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleTypeImportDeclaration x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.SingleTypeImportDeclaration"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.java.syntax.StatementExpression
statementExpressionAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementExpressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classInstanceCreation variant of hydra.java.syntax.StatementExpression
statementExpressionClassInstanceCreation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementExpressionClassInstanceCreation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstanceCreation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the methodInvocation variant of hydra.java.syntax.StatementExpression
statementExpressionMethodInvocation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementExpressionMethodInvocation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postDecrement variant of hydra.java.syntax.StatementExpression
statementExpressionPostDecrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementExpressionPostDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postIncrement variant of hydra.java.syntax.StatementExpression
statementExpressionPostIncrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementExpressionPostIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preDecrement variant of hydra.java.syntax.StatementExpression
statementExpressionPreDecrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementExpressionPreDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preIncrement variant of hydra.java.syntax.StatementExpression
statementExpressionPreIncrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementExpressionPreIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.java.syntax.Statement
statementFor :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ifThen variant of hydra.java.syntax.Statement
statementIfThen :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementIfThen x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThen"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ifThenElse variant of hydra.java.syntax.Statement
statementIfThenElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementIfThenElse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the labeled variant of hydra.java.syntax.Statement
statementLabeled :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementLabeled x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfFor :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementNoShortIfFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ifThenElse variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfIfThenElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementNoShortIfIfThenElse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the labeled variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfLabeled :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementNoShortIfLabeled x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfWhile :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementNoShortIfWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withoutTrailing variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfWithoutTrailing :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementNoShortIfWithoutTrailing x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.java.syntax.Statement
statementWhile :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withoutTrailing variant of hydra.java.syntax.Statement
statementWithoutTrailing :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailing x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assert variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementAssert :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementAssert x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assert"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the block variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the break variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBreak :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementBreak x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the continue variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementContinue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementContinue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the do variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementDo :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementDo x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the empty variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementEmpty :: Typed.TypedTerm t0
statementWithoutTrailingSubstatementEmpty =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the expression variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the return variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementReturn :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementReturn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the switch variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSwitch :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementSwitch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the synchronized variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSynchronized :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementSynchronized x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the throw variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementThrow :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementThrow x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the try variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementTry :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementTry x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the yield variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementYield :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementWithoutTrailingSubstatementYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.StaticImportOnDemandDeclaration wrapper
staticImportOnDemandDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
staticImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StaticImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.StaticInitializer wrapper
staticInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
staticInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StaticInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.StringLiteral wrapper
stringLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
stringLiteral x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StringLiteral"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the legacy variant of hydra.java.syntax.SwitchBlock
switchBlockLegacy :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchBlockLegacy x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "legacy"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rules variant of hydra.java.syntax.SwitchBlock
switchBlockRules :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchBlockRules x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rules"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroup :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchBlockStatementGroup labels statements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Typed.unTypedTerm labels)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm statements)}]}))
-- | DSL accessor for the labels field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupLabels :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchBlockStatementGroupLabels x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionFieldName = (Core.Name "labels")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statements field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupStatements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchBlockStatementGroupStatements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the labels field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithLabels :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchBlockStatementGroupWithLabels original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithStatements :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchBlockStatementGroupWithStatements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
              Core.projectionFieldName = (Core.Name "labels")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.SwitchBlock_Legacy
switchBlock_Legacy :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchBlock_Legacy groups trailingLabels =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Typed.unTypedTerm groups)},
        Core.Field {
          Core.fieldName = (Core.Name "trailingLabels"),
          Core.fieldTerm = (Typed.unTypedTerm trailingLabels)}]}))
-- | DSL accessor for the groups field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyGroups :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchBlock_LegacyGroups x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
        Core.projectionFieldName = (Core.Name "groups")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the trailingLabels field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyTrailingLabels :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchBlock_LegacyTrailingLabels x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
        Core.projectionFieldName = (Core.Name "trailingLabels")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the groups field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyWithGroups :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchBlock_LegacyWithGroups original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trailingLabels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
              Core.projectionFieldName = (Core.Name "trailingLabels")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the trailingLabels field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyWithTrailingLabels :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchBlock_LegacyWithTrailingLabels original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
              Core.projectionFieldName = (Core.Name "groups")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trailingLabels"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.SwitchExpression
switchExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchExpression cond block =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.SwitchExpression
switchExpressionBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchExpressionBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.SwitchExpression
switchExpressionCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchExpressionCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SwitchExpression
switchExpressionWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchExpressionWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.SwitchExpression
switchExpressionWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchExpressionWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the case variant of hydra.java.syntax.SwitchLabel
switchLabelCase :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchLabelCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the casePattern variant of hydra.java.syntax.SwitchLabel
switchLabelCasePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchLabelCasePattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "casePattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the default variant of hydra.java.syntax.SwitchLabel
switchLabelDefault :: Typed.TypedTerm t0
switchLabelDefault =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the null variant of hydra.java.syntax.SwitchLabel
switchLabelNull :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchLabelNull x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchRule
switchRule :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchRule label body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.SwitchRule
switchRuleBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchRuleBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.java.syntax.SwitchRule
switchRuleLabel :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchRuleLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.SwitchRule
switchRuleWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchRuleWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the label field of hydra.java.syntax.SwitchRule
switchRuleWithLabel :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchRuleWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the block variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchRule_BodyBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchRule_BodyExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the throw variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyThrow :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchRule_BodyThrow x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchStatement
switchStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchStatement cond block =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.SwitchStatement
switchStatementBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchStatementBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.SwitchStatement
switchStatementCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
switchStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SwitchStatement
switchStatementWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchStatementWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.SwitchStatement
switchStatementWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
switchStatementWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.SynchronizedStatement
synchronizedStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
synchronizedStatement expression block =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
synchronizedStatementBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
synchronizedStatementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
synchronizedStatementWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
synchronizedStatementWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.TextBlock wrapper
textBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
textBlock x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TextBlock"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.ThrowStatement wrapper
throwStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
throwStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ThrowStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.Throws wrapper
throws :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
throws x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Throws"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the class variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
topLevelClassOrInterfaceDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
topLevelClassOrInterfaceDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationNone :: Typed.TypedTerm t0
topLevelClassOrInterfaceDeclarationNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
topLevelClassOrInterfaceDeclarationWithComments value comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
topLevelClassOrInterfaceDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
topLevelClassOrInterfaceDeclarationWithCommentsValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsWithComments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
topLevelClassOrInterfaceDeclarationWithCommentsWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
topLevelClassOrInterfaceDeclarationWithCommentsWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the simple variant of hydra.java.syntax.TryStatement
tryStatementSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatementSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withFinally variant of hydra.java.syntax.TryStatement
tryStatementWithFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatementWithFinally x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withFinally"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withResources variant of hydra.java.syntax.TryStatement
tryStatementWithResources :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatementWithResources x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withResources"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TryStatement_Simple
tryStatement_Simple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryStatement_Simple block catches =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Typed.unTypedTerm catches)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatement_SimpleBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleCatches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatement_SimpleCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
        Core.projectionFieldName = (Core.Name "catches")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryStatement_SimpleWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
              Core.projectionFieldName = (Core.Name "catches")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the catches field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleWithCatches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryStatement_SimpleWithCatches original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
tryStatement_WithFinally block catches finally =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Typed.unTypedTerm catches)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm finally)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatement_WithFinallyBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyCatches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatement_WithFinallyCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionFieldName = (Core.Name "catches")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatement_WithFinallyFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryStatement_WithFinallyWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionFieldName = (Core.Name "catches")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the catches field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithCatches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryStatement_WithFinallyWithCatches original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryStatement_WithFinallyWithFinally original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionFieldName = (Core.Name "catches")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
tryWithResourcesStatement resourceSpecification block catches finally =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Typed.unTypedTerm resourceSpecification)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Typed.unTypedTerm catches)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm finally)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryWithResourcesStatementBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementCatches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryWithResourcesStatementCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "catches")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryWithResourcesStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the resourceSpecification field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementResourceSpecification :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryWithResourcesStatementResourceSpecification x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "resourceSpecification")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryWithResourcesStatementWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "resourceSpecification")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "catches")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the catches field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithCatches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryWithResourcesStatementWithCatches original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "resourceSpecification")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryWithResourcesStatementWithFinally original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "resourceSpecification")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "catches")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the resourceSpecification field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithResourceSpecification :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
tryWithResourcesStatementWithResourceSpecification original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "catches")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the reference variant of hydra.java.syntax.TypeArgument
typeArgumentReference :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeArgumentReference x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.java.syntax.TypeArgument
typeArgumentWildcard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeArgumentWildcard x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the arguments variant of hydra.java.syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeArgumentsOrDiamondArguments x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arguments"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the diamond variant of hydra.java.syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondDiamond :: Typed.TypedTerm t0
typeArgumentsOrDiamondDiamond =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "diamond"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.TypeBound
typeBoundClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeBoundClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.TypeBound
typeBoundVariable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeBoundVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeBound_ClassOrInterface type_ additional =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Typed.unTypedTerm additional)}]}))
-- | DSL accessor for the additional field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceAdditional :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeBound_ClassOrInterfaceAdditional x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "additional")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeBound_ClassOrInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the additional field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithAdditional :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeBound_ClassOrInterfaceWithAdditional original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeBound_ClassOrInterfaceWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
              Core.projectionFieldName = (Core.Name "additional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.TypeIdentifier wrapper
typeIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeIdentifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeIdentifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.TypeImportOnDemandDeclaration wrapper
typeImportOnDemandDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.TypeName
typeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeName identifier qualifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm qualifier)}]}))
-- | DSL injection for the array variant of hydra.java.syntax.TypeNameArray
typeNameArrayArray :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeNameArrayArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.TypeNameArray
typeNameArraySimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeNameArraySimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeName
typeNameIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeNameIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.TypeName
typeNameQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeNameQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.TypeName
typeNameWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeNameWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
              Core.projectionFieldName = (Core.Name "qualifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.TypeName
typeNameWithQualifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeNameWithQualifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.TypeParameter
typeParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
typeParameter modifiers identifier bound =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Typed.unTypedTerm bound)}]}))
-- | DSL accessor for the bound field of hydra.java.syntax.TypeParameter
typeParameterBound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeParameterBound x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "bound")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeParameter
typeParameterIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeParameterIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.TypeParameterModifier wrapper
typeParameterModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeParameterModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeParameterModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.TypeParameter
typeParameterModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeParameterModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bound field of hydra.java.syntax.TypeParameter
typeParameterWithBound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeParameterWithBound original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.TypeParameter
typeParameterWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeParameterWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "bound")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.TypeParameter
typeParameterWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeParameterWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "bound")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.TypePattern wrapper
typePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the primitive variant of hydra.java.syntax.Type
typePrimitive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typePrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the reference variant of hydra.java.syntax.Type
typeReference :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeReference x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TypeVariable
typeVariable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeVariable annotations identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.TypeVariable
typeVariableAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeVariableAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeVariable
typeVariableIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeVariableIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.TypeVariable
typeVariableWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeVariableWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.TypeVariable
typeVariableWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
typeVariableWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.java.syntax.AdditionalBound
unAdditionalBound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unAdditionalBound x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AdditionalBound")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AmbiguousName
unAmbiguousName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unAmbiguousName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AmbiguousName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AndExpression
unAndExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unAndExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AndExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AnnotationInterfaceBody
unAnnotationInterfaceBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unAnnotationInterfaceBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AnnotationInterfaceBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ArrayInitializer
unArrayInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unArrayInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ArrayInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Block
unBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Block")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.BreakStatement
unBreakStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unBreakStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.BreakStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.CaseConstant
unCaseConstant :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unCaseConstant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.CaseConstant")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Catches
unCatches :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Catches")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ClassBody
unClassBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unClassBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ClassBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConditionalAndExpression
unConditionalAndExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unConditionalAndExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConditionalAndExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConditionalOrExpression
unConditionalOrExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unConditionalOrExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConditionalOrExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConstantExpression
unConstantExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unConstantExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConstantExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ContinueStatement
unContinueStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unContinueStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ContinueStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.DefaultValue
unDefaultValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDefaultValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.DefaultValue")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Dims
unDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Dims")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ElementValueArrayInitializer
unElementValueArrayInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unElementValueArrayInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ElementValueArrayInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.EnumBody
unEnumBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unEnumBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.EnumBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.EnumConstantModifier
unEnumConstantModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unEnumConstantModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.EnumConstantModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ExclusiveOrExpression
unExclusiveOrExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unExclusiveOrExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ExclusiveOrExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ExpressionStatement
unExpressionStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unExpressionStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ExpressionStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Finally
unFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Finally")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.FloatingPointLiteral
unFloatingPointLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unFloatingPointLiteral x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.FloatingPointLiteral")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ForUpdate
unForUpdate :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unForUpdate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ForUpdate")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Guard
unGuard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Guard")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Identifier
unIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Identifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InclusiveOrExpression
unInclusiveOrExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unInclusiveOrExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InclusiveOrExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InstanceInitializer
unInstanceInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unInstanceInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InstanceInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.IntegerLiteral
unIntegerLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unIntegerLiteral x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.IntegerLiteral")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InterfaceBody
unInterfaceBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unInterfaceBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InterfaceBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InterfaceType
unInterfaceType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InterfaceType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.LocalVariableDeclarationStatement
unLocalVariableDeclarationStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unLocalVariableDeclarationStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.LocalVariableDeclarationStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MarkerAnnotation
unMarkerAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unMarkerAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MarkerAnnotation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MethodName
unMethodName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unMethodName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MethodName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MethodReference_Array
unMethodReference_Array :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unMethodReference_Array x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MethodReference_Array")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageModifier
unPackageModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPackageModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageName
unPackageName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPackageName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageOrTypeName
unPackageOrTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPackageOrTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageOrTypeName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PostDecrementExpression
unPostDecrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPostDecrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PostDecrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PostIncrementExpression
unPostIncrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPostIncrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PostIncrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PreDecrementExpression
unPreDecrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPreDecrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PreDecrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PreIncrementExpression
unPreIncrementExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPreIncrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PreIncrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordBody
unRecordBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unRecordBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordComponentModifier
unRecordComponentModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unRecordComponentModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordComponentModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordHeader
unRecordHeader :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unRecordHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordHeader")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ResourceSpecification
unResourceSpecification :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unResourceSpecification x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ResourceSpecification")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ReturnStatement
unReturnStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unReturnStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ReturnStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.SimpleTypeName
unSimpleTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unSimpleTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.SimpleTypeName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.SingleTypeImportDeclaration
unSingleTypeImportDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unSingleTypeImportDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.SingleTypeImportDeclaration")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StaticImportOnDemandDeclaration
unStaticImportOnDemandDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStaticImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StaticImportOnDemandDeclaration")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StaticInitializer
unStaticInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStaticInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StaticInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StringLiteral
unStringLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStringLiteral x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StringLiteral")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TextBlock
unTextBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unTextBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TextBlock")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ThrowStatement
unThrowStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unThrowStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ThrowStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Throws
unThrows :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unThrows x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Throws")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeIdentifier
unTypeIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unTypeIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeIdentifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeImportOnDemandDeclaration
unTypeImportOnDemandDeclaration :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unTypeImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeImportOnDemandDeclaration")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeParameterModifier
unTypeParameterModifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unTypeParameterModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeParameterModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypePattern
unTypePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unTypePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.UnannClassType
unUnannClassType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unUnannClassType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.UnannClassType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.UnannType
unUnannType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unUnannType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.UnannType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.YieldStatement
unYieldStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unYieldStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.YieldStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.UnannClassType wrapper
unannClassType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unannClassType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.UnannClassType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.UnannType wrapper
unannType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unannType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.UnannType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the minus variant of hydra.java.syntax.UnaryExpression
unaryExpressionMinus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionMinus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the cast variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusCast :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionNotPlusMinusCast x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the not variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusNot :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionNotPlusMinusNot x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postfix variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusPostfix :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionNotPlusMinusPostfix x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postfix"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the switchExpression variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusSwitchExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionNotPlusMinusSwitchExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switchExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tilde variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusTilde :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionNotPlusMinusTilde x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tilde"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the other variant of hydra.java.syntax.UnaryExpression
unaryExpressionOther :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the plus variant of hydra.java.syntax.UnaryExpression
unaryExpressionPlus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionPlus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preDecrement variant of hydra.java.syntax.UnaryExpression
unaryExpressionPreDecrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionPreDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preIncrement variant of hydra.java.syntax.UnaryExpression
unaryExpressionPreIncrement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unaryExpressionPreIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
unqualifiedClassInstanceCreationExpression typeArguments classOrInterface arguments body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Typed.unTypedTerm classOrInterface)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unqualifiedClassInstanceCreationExpressionArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unqualifiedClassInstanceCreationExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the classOrInterface field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unqualifiedClassInstanceCreationExpressionClassOrInterface x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "classOrInterface")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unqualifiedClassInstanceCreationExpressionTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
unqualifiedClassInstanceCreationExpressionWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "classOrInterface")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
unqualifiedClassInstanceCreationExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "classOrInterface")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the classOrInterface field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithClassOrInterface :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
unqualifiedClassInstanceCreationExpressionWithClassOrInterface original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithTypeArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
unqualifiedClassInstanceCreationExpressionWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "classOrInterface")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the expressionName variant of hydra.java.syntax.VariableAccess
variableAccessExpressionName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableAccessExpressionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.VariableAccess
variableAccessFieldAccess :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableAccessFieldAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.VariableArityParameter
variableArityParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
variableArityParameter modifiers type_ annotations identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.VariableArityParameter
variableArityParameterAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityParameterAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableArityParameter
variableArityParameterIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityParameterIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.VariableArityParameter
variableArityParameterModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityParameterModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.VariableArityParameter
variableArityParameterType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityParameterType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityParameterWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityParameterWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityParameterWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityParameterWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponent :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
variableArityRecordComponent modifiers type_ annotations identifier =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityRecordComponentAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityRecordComponentIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityRecordComponentModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableArityRecordComponentType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityRecordComponentWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityRecordComponentWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithModifiers :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityRecordComponentWithModifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableArityRecordComponentWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "modifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.VariableDeclarator
variableDeclarator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableDeclarator id initializer =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Typed.unTypedTerm initializer)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.VariableDeclarator
variableDeclaratorId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableDeclaratorId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.VariableDeclaratorId
variableDeclaratorId2 :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableDeclaratorId2 identifier dims =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm dims)}]}))
-- | DSL accessor for the dims field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableDeclaratorIdDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableDeclaratorIdIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dims field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdWithDims :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableDeclaratorIdWithDims original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
              Core.projectionFieldName = (Core.Name "identifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdWithIdentifier :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableDeclaratorIdWithIdentifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
              Core.projectionFieldName = (Core.Name "dims")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the initializer field of hydra.java.syntax.VariableDeclarator
variableDeclaratorInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableDeclaratorInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
        Core.projectionFieldName = (Core.Name "initializer")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.VariableDeclarator
variableDeclaratorWithId :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableDeclaratorWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
              Core.projectionFieldName = (Core.Name "initializer")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the initializer field of hydra.java.syntax.VariableDeclarator
variableDeclaratorWithInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
variableDeclaratorWithInitializer original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the arrayInitializer variant of hydra.java.syntax.VariableInitializer
variableInitializerArrayInitializer :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableInitializerArrayInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.VariableInitializer
variableInitializerExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableInitializerExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.VariableModifier
variableModifierAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
variableModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.VariableModifier
variableModifierFinal :: Typed.TypedTerm t0
variableModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.WhileStatement
whileStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
whileStatement cond body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.WhileStatement
whileStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
whileStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.WhileStatement
whileStatementCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
whileStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
whileStatementNoShortIf cond body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
whileStatementNoShortIfBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
whileStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
whileStatementNoShortIfWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
whileStatementNoShortIfWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.WhileStatement
whileStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
whileStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.WhileStatement
whileStatementWithCond :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
whileStatementWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.Wildcard
wildcard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
wildcard annotations wildcard =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Typed.unTypedTerm wildcard)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.Wildcard
wildcardAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
wildcardAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the extends variant of hydra.java.syntax.WildcardBounds
wildcardBoundsExtends :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
wildcardBoundsExtends x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extends"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.WildcardBounds
wildcardBoundsSuper :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
wildcardBoundsSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the wildcard field of hydra.java.syntax.Wildcard
wildcardWildcard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
wildcardWildcard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
        Core.projectionFieldName = (Core.Name "wildcard")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.Wildcard
wildcardWithAnnotations :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
wildcardWithAnnotations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
              Core.projectionFieldName = (Core.Name "wildcard")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the wildcard field of hydra.java.syntax.Wildcard
wildcardWithWildcard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
wildcardWithWildcard original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.YieldStatement wrapper
yieldStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
yieldStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.YieldStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
