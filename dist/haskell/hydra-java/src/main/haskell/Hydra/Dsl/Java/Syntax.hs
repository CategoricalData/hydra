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
additionalBound :: Typed.TypedTerm Syntax.InterfaceType -> Typed.TypedTerm Syntax.AdditionalBound
additionalBound x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AdditionalBound"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the minus variant of hydra.java.syntax.AdditiveExpression
additiveExpressionMinus :: Typed.TypedTerm Syntax.AdditiveExpression_Binary -> Typed.TypedTerm Syntax.AdditiveExpression
additiveExpressionMinus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the plus variant of hydra.java.syntax.AdditiveExpression
additiveExpressionPlus :: Typed.TypedTerm Syntax.AdditiveExpression_Binary -> Typed.TypedTerm Syntax.AdditiveExpression
additiveExpressionPlus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.AdditiveExpression
additiveExpressionUnary :: Typed.TypedTerm Syntax.MultiplicativeExpression -> Typed.TypedTerm Syntax.AdditiveExpression
additiveExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_Binary :: Typed.TypedTerm Syntax.AdditiveExpression -> Typed.TypedTerm Syntax.MultiplicativeExpression -> Typed.TypedTerm Syntax.AdditiveExpression_Binary
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
additiveExpression_BinaryLhs :: Typed.TypedTerm Syntax.AdditiveExpression_Binary -> Typed.TypedTerm Syntax.AdditiveExpression
additiveExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryRhs :: Typed.TypedTerm Syntax.AdditiveExpression_Binary -> Typed.TypedTerm Syntax.MultiplicativeExpression
additiveExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithLhs :: Typed.TypedTerm Syntax.AdditiveExpression_Binary -> Typed.TypedTerm Syntax.AdditiveExpression -> Typed.TypedTerm Syntax.AdditiveExpression_Binary
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
additiveExpression_BinaryWithRhs :: Typed.TypedTerm Syntax.AdditiveExpression_Binary -> Typed.TypedTerm Syntax.MultiplicativeExpression -> Typed.TypedTerm Syntax.AdditiveExpression_Binary
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
ambiguousName :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.AmbiguousName
ambiguousName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AmbiguousName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.AndExpression wrapper
andExpression :: Typed.TypedTerm [Syntax.EqualityExpression] -> Typed.TypedTerm Syntax.AndExpression
andExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AndExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifier :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.AnnotatedIdentifier
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
annotatedIdentifierAnnotations :: Typed.TypedTerm Syntax.AnnotatedIdentifier -> Typed.TypedTerm [Syntax.Annotation]
annotatedIdentifierAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierIdentifier :: Typed.TypedTerm Syntax.AnnotatedIdentifier -> Typed.TypedTerm Syntax.Identifier
annotatedIdentifierIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierWithAnnotations :: Typed.TypedTerm Syntax.AnnotatedIdentifier -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.AnnotatedIdentifier
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
annotatedIdentifierWithIdentifier :: Typed.TypedTerm Syntax.AnnotatedIdentifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.AnnotatedIdentifier
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
annotationInterfaceBody :: Typed.TypedTerm [Syntax.AnnotationInterfaceMemberDeclaration] -> Typed.TypedTerm Syntax.AnnotationInterfaceBody
annotationInterfaceBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclaration :: Typed.TypedTerm [Syntax.InterfaceModifier] -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.AnnotationInterfaceBody -> Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration
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
annotationInterfaceDeclarationBody :: Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration -> Typed.TypedTerm Syntax.AnnotationInterfaceBody
annotationInterfaceDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationIdentifier :: Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier
annotationInterfaceDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationModifiers :: Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration -> Typed.TypedTerm [Syntax.InterfaceModifier]
annotationInterfaceDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithBody :: Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration -> Typed.TypedTerm Syntax.AnnotationInterfaceBody -> Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration
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
annotationInterfaceDeclarationWithIdentifier :: Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration
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
annotationInterfaceDeclarationWithModifiers :: Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration -> Typed.TypedTerm [Syntax.InterfaceModifier] -> Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration
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
annotationInterfaceElementDeclaration :: Typed.TypedTerm [Syntax.AnnotationInterfaceElementModifier] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm (Maybe Syntax.DefaultValue) -> Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration
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
annotationInterfaceElementDeclarationDefault :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm (Maybe Syntax.DefaultValue)
annotationInterfaceElementDeclarationDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationDims :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm (Maybe Syntax.Dims)
annotationInterfaceElementDeclarationDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationIdentifier :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm Syntax.Identifier
annotationInterfaceElementDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationModifiers :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm [Syntax.AnnotationInterfaceElementModifier]
annotationInterfaceElementDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationType :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm Syntax.UnannType
annotationInterfaceElementDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithDefault :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm (Maybe Syntax.DefaultValue) -> Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration
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
annotationInterfaceElementDeclarationWithDims :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration
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
annotationInterfaceElementDeclarationWithIdentifier :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration
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
annotationInterfaceElementDeclarationWithModifiers :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm [Syntax.AnnotationInterfaceElementModifier] -> Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration
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
annotationInterfaceElementDeclarationWithType :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration
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
annotationInterfaceElementModifierAbstract :: Typed.TypedTerm Syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the public variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierPublic :: Typed.TypedTerm Syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotationInterface variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationAnnotationInterface :: Typed.TypedTerm Syntax.AnnotationInterfaceElementDeclaration -> Typed.TypedTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationAnnotationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationClass :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constant variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationConstant :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationConstant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationInterface :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the marker variant of hydra.java.syntax.Annotation
annotationMarker :: Typed.TypedTerm Syntax.MarkerAnnotation -> Typed.TypedTerm Syntax.Annotation
annotationMarker x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "marker"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.java.syntax.Annotation
annotationNormal :: Typed.TypedTerm Syntax.NormalAnnotation -> Typed.TypedTerm Syntax.Annotation
annotationNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the singleElement variant of hydra.java.syntax.Annotation
annotationSingleElement :: Typed.TypedTerm Syntax.SingleElementAnnotation -> Typed.TypedTerm Syntax.Annotation
annotationSingleElement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleElement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayAccess
arrayAccess :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ArrayAccess_Variant -> Typed.TypedTerm Syntax.ArrayAccess
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
arrayAccessExpression :: Typed.TypedTerm Syntax.ArrayAccess -> Typed.TypedTerm (Maybe Syntax.Expression)
arrayAccessExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ArrayAccess
arrayAccessVariant :: Typed.TypedTerm Syntax.ArrayAccess -> Typed.TypedTerm Syntax.ArrayAccess_Variant
arrayAccessVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.ArrayAccess
arrayAccessWithExpression :: Typed.TypedTerm Syntax.ArrayAccess -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ArrayAccess
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
arrayAccessWithVariant :: Typed.TypedTerm Syntax.ArrayAccess -> Typed.TypedTerm Syntax.ArrayAccess_Variant -> Typed.TypedTerm Syntax.ArrayAccess
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
arrayAccess_VariantArrayCreationWithInitializer :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer -> Typed.TypedTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantArrayCreationWithInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayCreationWithInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantName :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantPrimary :: Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression -> Typed.TypedTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withInitializer variant of hydra.java.syntax.ArrayCreationExpression
arrayCreationExpressionWithInitializer :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer -> Typed.TypedTerm Syntax.ArrayCreationExpression
arrayCreationExpressionWithInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerClassOrInterface :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerPrimitive :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterface :: Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm [Syntax.Dims] -> Typed.TypedTerm Syntax.ArrayInitializer -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
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
arrayCreationExpressionWithInitializer_ClassOrInterfaceArray :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ArrayInitializer
arrayCreationExpressionWithInitializer_ClassOrInterfaceArray x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "array")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Typed.TypedTerm [Syntax.Dims]
arrayCreationExpressionWithInitializer_ClassOrInterfaceDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ClassOrInterfaceType
arrayCreationExpressionWithInitializer_ClassOrInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithArray :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ArrayInitializer -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
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
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Typed.TypedTerm [Syntax.Dims] -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
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
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
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
arrayCreationExpressionWithInitializer_Primitive :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm [Syntax.Dims] -> Typed.TypedTerm Syntax.ArrayInitializer -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
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
arrayCreationExpressionWithInitializer_PrimitiveArray :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Typed.TypedTerm Syntax.ArrayInitializer
arrayCreationExpressionWithInitializer_PrimitiveArray x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "array")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Typed.TypedTerm [Syntax.Dims]
arrayCreationExpressionWithInitializer_PrimitiveDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations
arrayCreationExpressionWithInitializer_PrimitiveType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithArray :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Typed.TypedTerm Syntax.ArrayInitializer -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
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
arrayCreationExpressionWithInitializer_PrimitiveWithDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Typed.TypedTerm [Syntax.Dims] -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
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
arrayCreationExpressionWithInitializer_PrimitiveWithType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
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
arrayCreationExpressionWithoutInitializer :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer -> Typed.TypedTerm Syntax.ArrayCreationExpression
arrayCreationExpressionWithoutInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerClassOrInterface :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerPrimitive :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterface :: Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm [Syntax.DimExpr] -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
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
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDimExprs :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Typed.TypedTerm [Syntax.DimExpr]
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDimExprs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "dimExprs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Typed.TypedTerm (Maybe Syntax.Dims)
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ClassOrInterfaceType
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDimExprs :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Typed.TypedTerm [Syntax.DimExpr] -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
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
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
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
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
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
arrayCreationExpressionWithoutInitializer_Primitive :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm [Syntax.DimExpr] -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
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
arrayCreationExpressionWithoutInitializer_PrimitiveDimExprs :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Typed.TypedTerm [Syntax.DimExpr]
arrayCreationExpressionWithoutInitializer_PrimitiveDimExprs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "dimExprs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Typed.TypedTerm (Maybe Syntax.Dims)
arrayCreationExpressionWithoutInitializer_PrimitiveDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations
arrayCreationExpressionWithoutInitializer_PrimitiveType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithDimExprs :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Typed.TypedTerm [Syntax.DimExpr] -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
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
arrayCreationExpressionWithoutInitializer_PrimitiveWithDims :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
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
arrayCreationExpressionWithoutInitializer_PrimitiveWithType :: Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
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
arrayInitializer :: Typed.TypedTerm [[Syntax.VariableInitializer]] -> Typed.TypedTerm Syntax.ArrayInitializer
arrayInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ArrayInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.ArrayType
arrayType :: Typed.TypedTerm Syntax.Dims -> Typed.TypedTerm Syntax.ArrayType_Variant -> Typed.TypedTerm Syntax.ArrayType
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
arrayTypeDims :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.Dims
arrayTypeDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ArrayType
arrayTypeVariant :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.ArrayType_Variant
arrayTypeVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayType
arrayTypeWithDims :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.Dims -> Typed.TypedTerm Syntax.ArrayType
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
arrayTypeWithVariant :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.ArrayType_Variant -> Typed.TypedTerm Syntax.ArrayType
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
arrayType_VariantClassOrInterface :: Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm Syntax.ArrayType_Variant
arrayType_VariantClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantPrimitive :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.ArrayType_Variant
arrayType_VariantPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantVariable :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm Syntax.ArrayType_Variant
arrayType_VariantVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pair variant of hydra.java.syntax.AssertStatement
assertStatementPair :: Typed.TypedTerm Syntax.AssertStatement_Pair -> Typed.TypedTerm Syntax.AssertStatement
assertStatementPair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the single variant of hydra.java.syntax.AssertStatement
assertStatementSingle :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssertStatement
assertStatementSingle x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.AssertStatement_Pair
assertStatement_Pair :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssertStatement_Pair
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
assertStatement_PairFirst :: Typed.TypedTerm Syntax.AssertStatement_Pair -> Typed.TypedTerm Syntax.Expression
assertStatement_PairFirst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the second field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairSecond :: Typed.TypedTerm Syntax.AssertStatement_Pair -> Typed.TypedTerm Syntax.Expression
assertStatement_PairSecond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the first field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairWithFirst :: Typed.TypedTerm Syntax.AssertStatement_Pair -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssertStatement_Pair
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
assertStatement_PairWithSecond :: Typed.TypedTerm Syntax.AssertStatement_Pair -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssertStatement_Pair
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
assignment :: Typed.TypedTerm Syntax.LeftHandSide -> Typed.TypedTerm Syntax.AssignmentOperator -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Assignment
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
assignmentExpression :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.Expression
assignmentExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.java.syntax.AssignmentExpression
assignmentExpressionAssignment :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the conditional variant of hydra.java.syntax.AssignmentExpression
assignmentExpressionConditional :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpressionConditional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the lhs field of hydra.java.syntax.Assignment
assignmentLhs :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.LeftHandSide
assignmentLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.java.syntax.Assignment
assignmentOp :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.AssignmentOperator
assignmentOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the and variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorAnd :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorAnd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the div variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorDiv :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorDiv =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minus variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorMinus :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorMinus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mod variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorMod :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorMod =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the or variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorOr :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorOr =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorPlus :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorPlus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftLeft variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftLeft :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorShiftLeft =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftRight variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftRight :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorShiftRight =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftRightZeroFill variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftRightZeroFill :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorShiftRightZeroFill =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the simple variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorSimple :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorSimple =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the times variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorTimes :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorTimes =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the xor variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorXor :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorXor =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the expression field of hydra.java.syntax.Assignment
assignmentWithExpression :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Assignment
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
assignmentWithLhs :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.LeftHandSide -> Typed.TypedTerm Syntax.Assignment
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
assignmentWithOp :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.AssignmentOperator -> Typed.TypedTerm Syntax.Assignment
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
basicForStatement :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.BasicForStatement
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
basicForStatementBody :: Typed.TypedTerm Syntax.BasicForStatement -> Typed.TypedTerm Syntax.Statement
basicForStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.BasicForStatement
basicForStatementCond :: Typed.TypedTerm Syntax.BasicForStatement -> Typed.TypedTerm Syntax.ForCond
basicForStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIf :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.BasicForStatementNoShortIf
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
basicForStatementNoShortIfBody :: Typed.TypedTerm Syntax.BasicForStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
basicForStatementNoShortIfBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfCond :: Typed.TypedTerm Syntax.BasicForStatementNoShortIf -> Typed.TypedTerm Syntax.ForCond
basicForStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithBody :: Typed.TypedTerm Syntax.BasicForStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.BasicForStatementNoShortIf
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
basicForStatementNoShortIfWithCond :: Typed.TypedTerm Syntax.BasicForStatementNoShortIf -> Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm Syntax.BasicForStatementNoShortIf
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
basicForStatementWithBody :: Typed.TypedTerm Syntax.BasicForStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.BasicForStatement
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
basicForStatementWithCond :: Typed.TypedTerm Syntax.BasicForStatement -> Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm Syntax.BasicForStatement
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
block :: Typed.TypedTerm [Syntax.BlockStatement] -> Typed.TypedTerm Syntax.Block
block x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Block"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the localClassOrInterface variant of hydra.java.syntax.BlockStatement
blockStatementLocalClassOrInterface :: Typed.TypedTerm Syntax.LocalClassOrInterfaceDeclaration -> Typed.TypedTerm Syntax.BlockStatement
blockStatementLocalClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localClassOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the localVariableDeclaration variant of hydra.java.syntax.BlockStatement
blockStatementLocalVariableDeclaration :: Typed.TypedTerm Syntax.LocalVariableDeclarationStatement -> Typed.TypedTerm Syntax.BlockStatement
blockStatementLocalVariableDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariableDeclaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the statement variant of hydra.java.syntax.BlockStatement
blockStatementStatement :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.BlockStatement
blockStatementStatement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the array variant of hydra.java.syntax.BooleanArray
booleanArrayArray :: Typed.TypedTerm Syntax.BooleanArray -> Typed.TypedTerm Syntax.BooleanArray
booleanArrayArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.BooleanArray
booleanArraySimple :: Typed.TypedTerm Syntax.BooleanArray
booleanArraySimple =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.BreakStatement wrapper
breakStatement :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.BreakStatement
breakStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.BreakStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.CaseConstant wrapper
caseConstant :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.CaseConstant
caseConstant x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.CaseConstant"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.CasePattern
casePattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm (Maybe Syntax.Guard) -> Typed.TypedTerm Syntax.CasePattern
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
casePatternGuard :: Typed.TypedTerm Syntax.CasePattern -> Typed.TypedTerm (Maybe Syntax.Guard)
casePatternGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
        Core.projectionFieldName = (Core.Name "guard")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.java.syntax.CasePattern
casePatternPattern :: Typed.TypedTerm Syntax.CasePattern -> Typed.TypedTerm Syntax.Pattern
casePatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the guard field of hydra.java.syntax.CasePattern
casePatternWithGuard :: Typed.TypedTerm Syntax.CasePattern -> Typed.TypedTerm (Maybe Syntax.Guard) -> Typed.TypedTerm Syntax.CasePattern
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
casePatternWithPattern :: Typed.TypedTerm Syntax.CasePattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.CasePattern
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
castExpressionLambda :: Typed.TypedTerm Syntax.CastExpression_Lambda -> Typed.TypedTerm Syntax.CastExpression
castExpressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notPlusMinus variant of hydra.java.syntax.CastExpression
castExpressionNotPlusMinus :: Typed.TypedTerm Syntax.CastExpression_NotPlusMinus -> Typed.TypedTerm Syntax.CastExpression
castExpressionNotPlusMinus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notPlusMinus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.CastExpression
castExpressionPrimitive :: Typed.TypedTerm Syntax.CastExpression_Primitive -> Typed.TypedTerm Syntax.CastExpression
castExpressionPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.CastExpression_Lambda
castExpression_Lambda :: Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.CastExpression_Lambda
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
castExpression_LambdaExpression :: Typed.TypedTerm Syntax.CastExpression_Lambda -> Typed.TypedTerm Syntax.LambdaExpression
castExpression_LambdaExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the refAndBounds field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaRefAndBounds :: Typed.TypedTerm Syntax.CastExpression_Lambda -> Typed.TypedTerm Syntax.CastExpression_RefAndBounds
castExpression_LambdaRefAndBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
        Core.projectionFieldName = (Core.Name "refAndBounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaWithExpression :: Typed.TypedTerm Syntax.CastExpression_Lambda -> Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.CastExpression_Lambda
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
castExpression_LambdaWithRefAndBounds :: Typed.TypedTerm Syntax.CastExpression_Lambda -> Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm Syntax.CastExpression_Lambda
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
castExpression_NotPlusMinus :: Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.CastExpression_NotPlusMinus
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
castExpression_NotPlusMinusExpression :: Typed.TypedTerm Syntax.CastExpression_NotPlusMinus -> Typed.TypedTerm Syntax.UnaryExpression
castExpression_NotPlusMinusExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the refAndBounds field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusRefAndBounds :: Typed.TypedTerm Syntax.CastExpression_NotPlusMinus -> Typed.TypedTerm Syntax.CastExpression_RefAndBounds
castExpression_NotPlusMinusRefAndBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionFieldName = (Core.Name "refAndBounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithExpression :: Typed.TypedTerm Syntax.CastExpression_NotPlusMinus -> Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.CastExpression_NotPlusMinus
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
castExpression_NotPlusMinusWithRefAndBounds :: Typed.TypedTerm Syntax.CastExpression_NotPlusMinus -> Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm Syntax.CastExpression_NotPlusMinus
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
castExpression_Primitive :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.CastExpression_Primitive
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
castExpression_PrimitiveExpression :: Typed.TypedTerm Syntax.CastExpression_Primitive -> Typed.TypedTerm Syntax.UnaryExpression
castExpression_PrimitiveExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveType :: Typed.TypedTerm Syntax.CastExpression_Primitive -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations
castExpression_PrimitiveType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveWithExpression :: Typed.TypedTerm Syntax.CastExpression_Primitive -> Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.CastExpression_Primitive
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
castExpression_PrimitiveWithType :: Typed.TypedTerm Syntax.CastExpression_Primitive -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.CastExpression_Primitive
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
castExpression_RefAndBounds :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm [Syntax.AdditionalBound] -> Typed.TypedTerm Syntax.CastExpression_RefAndBounds
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
castExpression_RefAndBoundsBounds :: Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm [Syntax.AdditionalBound]
castExpression_RefAndBoundsBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionFieldName = (Core.Name "bounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsType :: Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm Syntax.ReferenceType
castExpression_RefAndBoundsType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bounds field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithBounds :: Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm [Syntax.AdditionalBound] -> Typed.TypedTerm Syntax.CastExpression_RefAndBounds
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
castExpression_RefAndBoundsWithType :: Typed.TypedTerm Syntax.CastExpression_RefAndBounds -> Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.CastExpression_RefAndBounds
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
catchClause :: Typed.TypedTerm (Maybe Syntax.CatchFormalParameter) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.CatchClause
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
catchClauseBlock :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm Syntax.Block
catchClauseBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameter field of hydra.java.syntax.CatchClause
catchClauseParameter :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm (Maybe Syntax.CatchFormalParameter)
catchClauseParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.CatchClause
catchClauseWithBlock :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.CatchClause
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
catchClauseWithParameter :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm (Maybe Syntax.CatchFormalParameter) -> Typed.TypedTerm Syntax.CatchClause
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
catchFormalParameter :: Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.CatchType -> Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.CatchFormalParameter
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
catchFormalParameterId :: Typed.TypedTerm Syntax.CatchFormalParameter -> Typed.TypedTerm Syntax.VariableDeclaratorId
catchFormalParameterId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterModifiers :: Typed.TypedTerm Syntax.CatchFormalParameter -> Typed.TypedTerm [Syntax.VariableModifier]
catchFormalParameterModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterType :: Typed.TypedTerm Syntax.CatchFormalParameter -> Typed.TypedTerm Syntax.CatchType
catchFormalParameterType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterWithId :: Typed.TypedTerm Syntax.CatchFormalParameter -> Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.CatchFormalParameter
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
catchFormalParameterWithModifiers :: Typed.TypedTerm Syntax.CatchFormalParameter -> Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.CatchFormalParameter
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
catchFormalParameterWithType :: Typed.TypedTerm Syntax.CatchFormalParameter -> Typed.TypedTerm Syntax.CatchType -> Typed.TypedTerm Syntax.CatchFormalParameter
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
catchType :: Typed.TypedTerm Syntax.UnannClassType -> Typed.TypedTerm [Syntax.ClassType] -> Typed.TypedTerm Syntax.CatchType
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
catchTypeType :: Typed.TypedTerm Syntax.CatchType -> Typed.TypedTerm Syntax.UnannClassType
catchTypeType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the types field of hydra.java.syntax.CatchType
catchTypeTypes :: Typed.TypedTerm Syntax.CatchType -> Typed.TypedTerm [Syntax.ClassType]
catchTypeTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the type field of hydra.java.syntax.CatchType
catchTypeWithType :: Typed.TypedTerm Syntax.CatchType -> Typed.TypedTerm Syntax.UnannClassType -> Typed.TypedTerm Syntax.CatchType
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
catchTypeWithTypes :: Typed.TypedTerm Syntax.CatchType -> Typed.TypedTerm [Syntax.ClassType] -> Typed.TypedTerm Syntax.CatchType
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
catches :: Typed.TypedTerm [Syntax.CatchClause] -> Typed.TypedTerm Syntax.Catches
catches x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Catches"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.ClassBody wrapper
classBody :: Typed.TypedTerm [Syntax.ClassBodyDeclarationWithComments] -> Typed.TypedTerm Syntax.ClassBody
classBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ClassBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the classMember variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationClassMember :: Typed.TypedTerm Syntax.ClassMemberDeclaration -> Typed.TypedTerm Syntax.ClassBodyDeclaration
classBodyDeclarationClassMember x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classMember"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constructorDeclaration variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationConstructorDeclaration :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm Syntax.ClassBodyDeclaration
classBodyDeclarationConstructorDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructorDeclaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the instanceInitializer variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationInstanceInitializer :: Typed.TypedTerm Syntax.InstanceInitializer -> Typed.TypedTerm Syntax.ClassBodyDeclaration
classBodyDeclarationInstanceInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the staticInitializer variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationStaticInitializer :: Typed.TypedTerm Syntax.StaticInitializer -> Typed.TypedTerm Syntax.ClassBodyDeclaration
classBodyDeclarationStaticInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithComments :: Typed.TypedTerm Syntax.ClassBodyDeclaration -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.ClassBodyDeclarationWithComments
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
classBodyDeclarationWithCommentsComments :: Typed.TypedTerm Syntax.ClassBodyDeclarationWithComments -> Typed.TypedTerm (Maybe String)
classBodyDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsValue :: Typed.TypedTerm Syntax.ClassBodyDeclarationWithComments -> Typed.TypedTerm Syntax.ClassBodyDeclaration
classBodyDeclarationWithCommentsValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithComments :: Typed.TypedTerm Syntax.ClassBodyDeclarationWithComments -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.ClassBodyDeclarationWithComments
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
classBodyDeclarationWithCommentsWithValue :: Typed.TypedTerm Syntax.ClassBodyDeclarationWithComments -> Typed.TypedTerm Syntax.ClassBodyDeclaration -> Typed.TypedTerm Syntax.ClassBodyDeclarationWithComments
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
classDeclarationEnum :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclarationEnum x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.java.syntax.ClassDeclaration
classDeclarationNormal :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclarationNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.java.syntax.ClassDeclaration
classDeclarationRecord :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclarationRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpression :: Typed.TypedTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier) -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm Syntax.ClassInstanceCreationExpression
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
classInstanceCreationExpressionExpression :: Typed.TypedTerm Syntax.ClassInstanceCreationExpression -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression
classInstanceCreationExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionQualifier :: Typed.TypedTerm Syntax.ClassInstanceCreationExpression -> Typed.TypedTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier)
classInstanceCreationExpressionQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithExpression :: Typed.TypedTerm Syntax.ClassInstanceCreationExpression -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm Syntax.ClassInstanceCreationExpression
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
classInstanceCreationExpressionWithQualifier :: Typed.TypedTerm Syntax.ClassInstanceCreationExpression -> Typed.TypedTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier) -> Typed.TypedTerm Syntax.ClassInstanceCreationExpression
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
classInstanceCreationExpression_QualifierExpression :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierPrimary :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.ClassLiteral
classLiteralBoolean :: Typed.TypedTerm Syntax.BooleanArray -> Typed.TypedTerm Syntax.ClassLiteral
classLiteralBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the numericType variant of hydra.java.syntax.ClassLiteral
classLiteralNumericType :: Typed.TypedTerm Syntax.NumericTypeArray -> Typed.TypedTerm Syntax.ClassLiteral
classLiteralNumericType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numericType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.ClassLiteral
classLiteralType :: Typed.TypedTerm Syntax.TypeNameArray -> Typed.TypedTerm Syntax.ClassLiteral
classLiteralType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the void variant of hydra.java.syntax.ClassLiteral
classLiteralVoid :: Typed.TypedTerm Syntax.ClassLiteral
classLiteralVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the class variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationClass :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.ClassMemberDeclaration
classMemberDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the field variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationField :: Typed.TypedTerm Syntax.FieldDeclaration -> Typed.TypedTerm Syntax.ClassMemberDeclaration
classMemberDeclarationField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "field"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationInterface :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm Syntax.ClassMemberDeclaration
classMemberDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the method variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationMethod :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm Syntax.ClassMemberDeclaration
classMemberDeclarationMethod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationNone :: Typed.TypedTerm Syntax.ClassMemberDeclaration
classMemberDeclarationNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.ClassModifier
classModifierAbstract :: Typed.TypedTerm Syntax.ClassModifier
classModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ClassModifier
classModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.ClassModifier
classModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.ClassModifier
classModifierFinal :: Typed.TypedTerm Syntax.ClassModifier
classModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nonSealed variant of hydra.java.syntax.ClassModifier
classModifierNonSealed :: Typed.TypedTerm Syntax.ClassModifier
classModifierNonSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonSealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.ClassModifier
classModifierPrivate :: Typed.TypedTerm Syntax.ClassModifier
classModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.ClassModifier
classModifierProtected :: Typed.TypedTerm Syntax.ClassModifier
classModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ClassModifier
classModifierPublic :: Typed.TypedTerm Syntax.ClassModifier
classModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sealed variant of hydra.java.syntax.ClassModifier
classModifierSealed :: Typed.TypedTerm Syntax.ClassModifier
classModifierSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.ClassModifier
classModifierStatic :: Typed.TypedTerm Syntax.ClassModifier
classModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.ClassModifier
classModifierStrictfp :: Typed.TypedTerm Syntax.ClassModifier
classModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the class variant of hydra.java.syntax.ClassOrInterfaceType
classOrInterfaceTypeClass :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.ClassOrInterfaceType
classOrInterfaceTypeClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.ClassOrInterfaceType
classOrInterfaceTypeInterface :: Typed.TypedTerm Syntax.InterfaceType -> Typed.TypedTerm Syntax.ClassOrInterfaceType
classOrInterfaceTypeInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate :: Typed.TypedTerm [Syntax.AnnotatedIdentifier] -> Typed.TypedTerm (Maybe Syntax.TypeArgumentsOrDiamond) -> Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate
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
classOrInterfaceTypeToInstantiateIdentifiers :: Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Typed.TypedTerm [Syntax.AnnotatedIdentifier]
classOrInterfaceTypeToInstantiateIdentifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionFieldName = (Core.Name "identifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateTypeArguments :: Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Typed.TypedTerm (Maybe Syntax.TypeArgumentsOrDiamond)
classOrInterfaceTypeToInstantiateTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifiers field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithIdentifiers :: Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Typed.TypedTerm [Syntax.AnnotatedIdentifier] -> Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate
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
classOrInterfaceTypeToInstantiateWithTypeArguments :: Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Typed.TypedTerm (Maybe Syntax.TypeArgumentsOrDiamond) -> Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate
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
classType :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.ClassTypeQualifier -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.ClassType
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
classTypeAnnotations :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm [Syntax.Annotation]
classTypeAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the arguments field of hydra.java.syntax.ClassType
classTypeArguments :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm [Syntax.TypeArgument]
classTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ClassType
classTypeIdentifier :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.TypeIdentifier
classTypeIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ClassType
classTypeQualifier :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.ClassTypeQualifier
classTypeQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the none variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierNone :: Typed.TypedTerm Syntax.ClassTypeQualifier
classTypeQualifierNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the package variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierPackage :: Typed.TypedTerm Syntax.PackageName -> Typed.TypedTerm Syntax.ClassTypeQualifier
classTypeQualifierPackage x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "package"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parent variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierParent :: Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm Syntax.ClassTypeQualifier
classTypeQualifierParent x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parent"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the annotations field of hydra.java.syntax.ClassType
classTypeWithAnnotations :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.ClassType
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
classTypeWithArguments :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.ClassType
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
classTypeWithIdentifier :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.ClassType
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
classTypeWithQualifier :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.ClassTypeQualifier -> Typed.TypedTerm Syntax.ClassType
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
compactConstructorDeclaration :: Typed.TypedTerm [Syntax.ConstructorModifier] -> Typed.TypedTerm Syntax.SimpleTypeName -> Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm Syntax.CompactConstructorDeclaration
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
compactConstructorDeclarationBody :: Typed.TypedTerm Syntax.CompactConstructorDeclaration -> Typed.TypedTerm Syntax.ConstructorBody
compactConstructorDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationModifiers :: Typed.TypedTerm Syntax.CompactConstructorDeclaration -> Typed.TypedTerm [Syntax.ConstructorModifier]
compactConstructorDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationName :: Typed.TypedTerm Syntax.CompactConstructorDeclaration -> Typed.TypedTerm Syntax.SimpleTypeName
compactConstructorDeclarationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithBody :: Typed.TypedTerm Syntax.CompactConstructorDeclaration -> Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm Syntax.CompactConstructorDeclaration
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
compactConstructorDeclarationWithModifiers :: Typed.TypedTerm Syntax.CompactConstructorDeclaration -> Typed.TypedTerm [Syntax.ConstructorModifier] -> Typed.TypedTerm Syntax.CompactConstructorDeclaration
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
compactConstructorDeclarationWithName :: Typed.TypedTerm Syntax.CompactConstructorDeclaration -> Typed.TypedTerm Syntax.SimpleTypeName -> Typed.TypedTerm Syntax.CompactConstructorDeclaration
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
compilationUnitModular :: Typed.TypedTerm Syntax.ModularCompilationUnit -> Typed.TypedTerm Syntax.CompilationUnit
compilationUnitModular x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modular"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ordinary variant of hydra.java.syntax.CompilationUnit
compilationUnitOrdinary :: Typed.TypedTerm Syntax.OrdinaryCompilationUnit -> Typed.TypedTerm Syntax.CompilationUnit
compilationUnitOrdinary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ConditionalAndExpression wrapper
conditionalAndExpression :: Typed.TypedTerm [Syntax.InclusiveOrExpression] -> Typed.TypedTerm Syntax.ConditionalAndExpression
conditionalAndExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConditionalAndExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionSimple :: Typed.TypedTerm Syntax.ConditionalOrExpression -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ternaryCond variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionTernaryCond :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpressionTernaryCond x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryCond"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ternaryLambda variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionTernaryLambda :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpressionTernaryLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryLambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCond :: Typed.TypedTerm Syntax.ConditionalOrExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond
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
conditionalExpression_TernaryCondCond :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond -> Typed.TypedTerm Syntax.ConditionalOrExpression
conditionalExpression_TernaryCondCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondIfFalse :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpression_TernaryCondIfFalse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionFieldName = (Core.Name "ifFalse")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondIfTrue :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond -> Typed.TypedTerm Syntax.Expression
conditionalExpression_TernaryCondIfTrue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionFieldName = (Core.Name "ifTrue")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithCond :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond -> Typed.TypedTerm Syntax.ConditionalOrExpression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond
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
conditionalExpression_TernaryCondWithIfFalse :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond -> Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond
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
conditionalExpression_TernaryCondWithIfTrue :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryCond
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
conditionalExpression_TernaryLambda :: Typed.TypedTerm Syntax.ConditionalOrExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda
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
conditionalExpression_TernaryLambdaCond :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda -> Typed.TypedTerm Syntax.ConditionalOrExpression
conditionalExpression_TernaryLambdaCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaIfFalse :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda -> Typed.TypedTerm Syntax.LambdaExpression
conditionalExpression_TernaryLambdaIfFalse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionFieldName = (Core.Name "ifFalse")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaIfTrue :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda -> Typed.TypedTerm Syntax.Expression
conditionalExpression_TernaryLambdaIfTrue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionFieldName = (Core.Name "ifTrue")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithCond :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda -> Typed.TypedTerm Syntax.ConditionalOrExpression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda
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
conditionalExpression_TernaryLambdaWithIfFalse :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda -> Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda
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
conditionalExpression_TernaryLambdaWithIfTrue :: Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConditionalExpression_TernaryLambda
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
conditionalOrExpression :: Typed.TypedTerm [Syntax.ConditionalAndExpression] -> Typed.TypedTerm Syntax.ConditionalOrExpression
conditionalOrExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConditionalOrExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.ConstantDeclaration
constantDeclaration :: Typed.TypedTerm [Syntax.ConstantModifier] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.ConstantDeclaration
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
constantDeclarationModifiers :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm [Syntax.ConstantModifier]
constantDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ConstantDeclaration
constantDeclarationType :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm Syntax.UnannType
constantDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variables field of hydra.java.syntax.ConstantDeclaration
constantDeclarationVariables :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator]
constantDeclarationVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ConstantDeclaration
constantDeclarationWithModifiers :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm [Syntax.ConstantModifier] -> Typed.TypedTerm Syntax.ConstantDeclaration
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
constantDeclarationWithType :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.ConstantDeclaration
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
constantDeclarationWithVariables :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.ConstantDeclaration
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
constantExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConstantExpression
constantExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConstantExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ConstantModifier
constantModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.ConstantModifier
constantModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.ConstantModifier
constantModifierFinal :: Typed.TypedTerm Syntax.ConstantModifier
constantModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ConstantModifier
constantModifierPublic :: Typed.TypedTerm Syntax.ConstantModifier
constantModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.ConstantModifier
constantModifierStatic :: Typed.TypedTerm Syntax.ConstantModifier
constantModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.ConstructorBody
constructorBody :: Typed.TypedTerm (Maybe Syntax.ExplicitConstructorInvocation) -> Typed.TypedTerm [Syntax.BlockStatement] -> Typed.TypedTerm Syntax.ConstructorBody
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
constructorBodyInvocation :: Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm (Maybe Syntax.ExplicitConstructorInvocation)
constructorBodyInvocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
        Core.projectionFieldName = (Core.Name "invocation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statements field of hydra.java.syntax.ConstructorBody
constructorBodyStatements :: Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm [Syntax.BlockStatement]
constructorBodyStatements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the invocation field of hydra.java.syntax.ConstructorBody
constructorBodyWithInvocation :: Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm (Maybe Syntax.ExplicitConstructorInvocation) -> Typed.TypedTerm Syntax.ConstructorBody
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
constructorBodyWithStatements :: Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm [Syntax.BlockStatement] -> Typed.TypedTerm Syntax.ConstructorBody
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
constructorDeclaration :: Typed.TypedTerm [Syntax.ConstructorModifier] -> Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm (Maybe Syntax.Throws) -> Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm Syntax.ConstructorDeclaration
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
constructorDeclarationBody :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm Syntax.ConstructorBody
constructorDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constructor field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationConstructor :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm Syntax.ConstructorDeclarator
constructorDeclarationConstructor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "constructor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationModifiers :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm [Syntax.ConstructorModifier]
constructorDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the throws field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationThrows :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm (Maybe Syntax.Throws)
constructorDeclarationThrows x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionFieldName = (Core.Name "throws")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithBody :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm Syntax.ConstructorBody -> Typed.TypedTerm Syntax.ConstructorDeclaration
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
constructorDeclarationWithConstructor :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm Syntax.ConstructorDeclaration
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
constructorDeclarationWithModifiers :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm [Syntax.ConstructorModifier] -> Typed.TypedTerm Syntax.ConstructorDeclaration
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
constructorDeclarationWithThrows :: Typed.TypedTerm Syntax.ConstructorDeclaration -> Typed.TypedTerm (Maybe Syntax.Throws) -> Typed.TypedTerm Syntax.ConstructorDeclaration
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
constructorDeclarator :: Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.SimpleTypeName -> Typed.TypedTerm (Maybe Syntax.ReceiverParameter) -> Typed.TypedTerm [Syntax.FormalParameter] -> Typed.TypedTerm Syntax.ConstructorDeclarator
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
constructorDeclaratorFormalParameters :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm [Syntax.FormalParameter]
constructorDeclaratorFormalParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "formalParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorName :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm Syntax.SimpleTypeName
constructorDeclaratorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorParameters :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm [Syntax.TypeParameter]
constructorDeclaratorParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the receiverParameter field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorReceiverParameter :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm (Maybe Syntax.ReceiverParameter)
constructorDeclaratorReceiverParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionFieldName = (Core.Name "receiverParameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the formalParameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithFormalParameters :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm [Syntax.FormalParameter] -> Typed.TypedTerm Syntax.ConstructorDeclarator
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
constructorDeclaratorWithName :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm Syntax.SimpleTypeName -> Typed.TypedTerm Syntax.ConstructorDeclarator
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
constructorDeclaratorWithParameters :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.ConstructorDeclarator
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
constructorDeclaratorWithReceiverParameter :: Typed.TypedTerm Syntax.ConstructorDeclarator -> Typed.TypedTerm (Maybe Syntax.ReceiverParameter) -> Typed.TypedTerm Syntax.ConstructorDeclarator
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
constructorModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.ConstructorModifier
constructorModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the private variant of hydra.java.syntax.ConstructorModifier
constructorModifierPrivate :: Typed.TypedTerm Syntax.ConstructorModifier
constructorModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.ConstructorModifier
constructorModifierProtected :: Typed.TypedTerm Syntax.ConstructorModifier
constructorModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ConstructorModifier
constructorModifierPublic :: Typed.TypedTerm Syntax.ConstructorModifier
constructorModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.ContinueStatement wrapper
continueStatement :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.ContinueStatement
continueStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ContinueStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.DefaultValue wrapper
defaultValue :: Typed.TypedTerm Syntax.ElementValue -> Typed.TypedTerm Syntax.DefaultValue
defaultValue x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.DefaultValue"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.DimExpr
dimExpr :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.DimExpr
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
dimExprAnnotations :: Typed.TypedTerm Syntax.DimExpr -> Typed.TypedTerm [Syntax.Annotation]
dimExprAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.DimExpr
dimExprExpression :: Typed.TypedTerm Syntax.DimExpr -> Typed.TypedTerm (Maybe Syntax.Expression)
dimExprExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.DimExpr
dimExprWithAnnotations :: Typed.TypedTerm Syntax.DimExpr -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.DimExpr
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
dimExprWithExpression :: Typed.TypedTerm Syntax.DimExpr -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.DimExpr
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
dims :: Typed.TypedTerm [[Syntax.Annotation]] -> Typed.TypedTerm Syntax.Dims
dims x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Dims"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.DoStatement
doStatement :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DoStatement
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
doStatementBody :: Typed.TypedTerm Syntax.DoStatement -> Typed.TypedTerm Syntax.Statement
doStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.DoStatement
doStatementCond :: Typed.TypedTerm Syntax.DoStatement -> Typed.TypedTerm Syntax.Expression
doStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.DoStatement
doStatementWithBody :: Typed.TypedTerm Syntax.DoStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.DoStatement
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
doStatementWithCond :: Typed.TypedTerm Syntax.DoStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DoStatement
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
elementValueAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.ElementValue
elementValueAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ElementValueArrayInitializer wrapper
elementValueArrayInitializer :: Typed.TypedTerm [Syntax.ElementValue] -> Typed.TypedTerm Syntax.ElementValueArrayInitializer
elementValueArrayInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ElementValueArrayInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the conditionalExpression variant of hydra.java.syntax.ElementValue
elementValueConditionalExpression :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.ElementValue
elementValueConditionalExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditionalExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the elementValueArrayInitializer variant of hydra.java.syntax.ElementValue
elementValueElementValueArrayInitializer :: Typed.TypedTerm Syntax.ElementValueArrayInitializer -> Typed.TypedTerm Syntax.ElementValue
elementValueElementValueArrayInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elementValueArrayInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ElementValuePair
elementValuePair :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ElementValue -> Typed.TypedTerm Syntax.ElementValuePair
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
elementValuePairKey :: Typed.TypedTerm Syntax.ElementValuePair -> Typed.TypedTerm Syntax.Identifier
elementValuePairKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.ElementValuePair
elementValuePairValue :: Typed.TypedTerm Syntax.ElementValuePair -> Typed.TypedTerm Syntax.ElementValue
elementValuePairValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.java.syntax.ElementValuePair
elementValuePairWithKey :: Typed.TypedTerm Syntax.ElementValuePair -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ElementValuePair
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
elementValuePairWithValue :: Typed.TypedTerm Syntax.ElementValuePair -> Typed.TypedTerm Syntax.ElementValue -> Typed.TypedTerm Syntax.ElementValuePair
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
enhancedForCond :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.EnhancedForCond
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
enhancedForCondDeclaration :: Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.LocalVariableDeclaration
enhancedForCondDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
        Core.projectionFieldName = (Core.Name "declaration")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.EnhancedForCond
enhancedForCondExpression :: Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.Expression
enhancedForCondExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declaration field of hydra.java.syntax.EnhancedForCond
enhancedForCondWithDeclaration :: Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm Syntax.EnhancedForCond
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
enhancedForCondWithExpression :: Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.EnhancedForCond
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
enhancedForStatement :: Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.EnhancedForStatement
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
enhancedForStatementBody :: Typed.TypedTerm Syntax.EnhancedForStatement -> Typed.TypedTerm Syntax.Statement
enhancedForStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementCond :: Typed.TypedTerm Syntax.EnhancedForStatement -> Typed.TypedTerm Syntax.EnhancedForCond
enhancedForStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIf :: Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf
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
enhancedForStatementNoShortIfBody :: Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
enhancedForStatementNoShortIfBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfCond :: Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf -> Typed.TypedTerm Syntax.EnhancedForCond
enhancedForStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithBody :: Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf
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
enhancedForStatementNoShortIfWithCond :: Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf -> Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf
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
enhancedForStatementWithBody :: Typed.TypedTerm Syntax.EnhancedForStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.EnhancedForStatement
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
enhancedForStatementWithCond :: Typed.TypedTerm Syntax.EnhancedForStatement -> Typed.TypedTerm Syntax.EnhancedForCond -> Typed.TypedTerm Syntax.EnhancedForStatement
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
enumBody :: Typed.TypedTerm [Syntax.EnumBody_Element] -> Typed.TypedTerm Syntax.EnumBody
enumBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.EnumBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.EnumBody_Element
enumBody_Element :: Typed.TypedTerm [Syntax.EnumConstant] -> Typed.TypedTerm [Syntax.ClassBodyDeclaration] -> Typed.TypedTerm Syntax.EnumBody_Element
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
enumBody_ElementBodyDeclarations :: Typed.TypedTerm Syntax.EnumBody_Element -> Typed.TypedTerm [Syntax.ClassBodyDeclaration]
enumBody_ElementBodyDeclarations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
        Core.projectionFieldName = (Core.Name "bodyDeclarations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constants field of hydra.java.syntax.EnumBody_Element
enumBody_ElementConstants :: Typed.TypedTerm Syntax.EnumBody_Element -> Typed.TypedTerm [Syntax.EnumConstant]
enumBody_ElementConstants x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
        Core.projectionFieldName = (Core.Name "constants")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bodyDeclarations field of hydra.java.syntax.EnumBody_Element
enumBody_ElementWithBodyDeclarations :: Typed.TypedTerm Syntax.EnumBody_Element -> Typed.TypedTerm [Syntax.ClassBodyDeclaration] -> Typed.TypedTerm Syntax.EnumBody_Element
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
enumBody_ElementWithConstants :: Typed.TypedTerm Syntax.EnumBody_Element -> Typed.TypedTerm [Syntax.EnumConstant] -> Typed.TypedTerm Syntax.EnumBody_Element
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
enumConstant :: Typed.TypedTerm [Syntax.EnumConstantModifier] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe [Syntax.Expression]) -> Typed.TypedTerm (Maybe Syntax.ClassBody) -> Typed.TypedTerm Syntax.EnumConstant
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
enumConstantArguments :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm (Maybe [Syntax.Expression])
enumConstantArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.EnumConstant
enumConstantBody :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm (Maybe Syntax.ClassBody)
enumConstantBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.EnumConstant
enumConstantIdentifier :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm Syntax.Identifier
enumConstantIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.EnumConstantModifier wrapper
enumConstantModifier :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.EnumConstantModifier
enumConstantModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.EnumConstantModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.EnumConstant
enumConstantModifiers :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm [Syntax.EnumConstantModifier]
enumConstantModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.EnumConstant
enumConstantWithArguments :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm (Maybe [Syntax.Expression]) -> Typed.TypedTerm Syntax.EnumConstant
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
enumConstantWithBody :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm (Maybe Syntax.ClassBody) -> Typed.TypedTerm Syntax.EnumConstant
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
enumConstantWithIdentifier :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.EnumConstant
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
enumConstantWithModifiers :: Typed.TypedTerm Syntax.EnumConstant -> Typed.TypedTerm [Syntax.EnumConstantModifier] -> Typed.TypedTerm Syntax.EnumConstant
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
enumDeclaration :: Typed.TypedTerm [Syntax.ClassModifier] -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm Syntax.EnumBody -> Typed.TypedTerm Syntax.EnumDeclaration
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
enumDeclarationBody :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm Syntax.EnumBody
enumDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.EnumDeclaration
enumDeclarationIdentifier :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier
enumDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.EnumDeclaration
enumDeclarationImplements :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm [Syntax.InterfaceType]
enumDeclarationImplements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "implements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.EnumDeclaration
enumDeclarationModifiers :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm [Syntax.ClassModifier]
enumDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithBody :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm Syntax.EnumBody -> Typed.TypedTerm Syntax.EnumDeclaration
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
enumDeclarationWithIdentifier :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.EnumDeclaration
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
enumDeclarationWithImplements :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm Syntax.EnumDeclaration
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
enumDeclarationWithModifiers :: Typed.TypedTerm Syntax.EnumDeclaration -> Typed.TypedTerm [Syntax.ClassModifier] -> Typed.TypedTerm Syntax.EnumDeclaration
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
equalityExpressionEqual :: Typed.TypedTerm Syntax.EqualityExpression_Binary -> Typed.TypedTerm Syntax.EqualityExpression
equalityExpressionEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notEqual variant of hydra.java.syntax.EqualityExpression
equalityExpressionNotEqual :: Typed.TypedTerm Syntax.EqualityExpression_Binary -> Typed.TypedTerm Syntax.EqualityExpression
equalityExpressionNotEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.EqualityExpression
equalityExpressionUnary :: Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.EqualityExpression
equalityExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.EqualityExpression_Binary
equalityExpression_Binary :: Typed.TypedTerm Syntax.EqualityExpression -> Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.EqualityExpression_Binary
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
equalityExpression_BinaryLhs :: Typed.TypedTerm Syntax.EqualityExpression_Binary -> Typed.TypedTerm Syntax.EqualityExpression
equalityExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryRhs :: Typed.TypedTerm Syntax.EqualityExpression_Binary -> Typed.TypedTerm Syntax.RelationalExpression
equalityExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryWithLhs :: Typed.TypedTerm Syntax.EqualityExpression_Binary -> Typed.TypedTerm Syntax.EqualityExpression -> Typed.TypedTerm Syntax.EqualityExpression_Binary
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
equalityExpression_BinaryWithRhs :: Typed.TypedTerm Syntax.EqualityExpression_Binary -> Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.EqualityExpression_Binary
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
exceptionTypeClass :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.ExceptionType
exceptionTypeClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ExceptionType
exceptionTypeVariable :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm Syntax.ExceptionType
exceptionTypeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ExclusiveOrExpression wrapper
exclusiveOrExpression :: Typed.TypedTerm [Syntax.AndExpression] -> Typed.TypedTerm Syntax.ExclusiveOrExpression
exclusiveOrExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ExclusiveOrExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocation :: Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation_Variant -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation
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
explicitConstructorInvocationArguments :: Typed.TypedTerm Syntax.ExplicitConstructorInvocation -> Typed.TypedTerm [Syntax.Expression]
explicitConstructorInvocationArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationTypeArguments :: Typed.TypedTerm Syntax.ExplicitConstructorInvocation -> Typed.TypedTerm [Syntax.TypeArgument]
explicitConstructorInvocationTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationVariant :: Typed.TypedTerm Syntax.ExplicitConstructorInvocation -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocationVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithArguments :: Typed.TypedTerm Syntax.ExplicitConstructorInvocation -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation
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
explicitConstructorInvocationWithTypeArguments :: Typed.TypedTerm Syntax.ExplicitConstructorInvocation -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation
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
explicitConstructorInvocationWithVariant :: Typed.TypedTerm Syntax.ExplicitConstructorInvocation -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation_Variant -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation
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
explicitConstructorInvocation_VariantPrimary :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantSuper :: Typed.TypedTerm (Maybe Syntax.ExpressionName) -> Typed.TypedTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the this variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantThis :: Typed.TypedTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantThis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the assignment variant of hydra.java.syntax.Expression
expressionAssignment :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Expression
expressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.java.syntax.Expression
expressionLambda :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.Expression
expressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ExpressionName
expressionName :: Typed.TypedTerm (Maybe Syntax.AmbiguousName) -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ExpressionName
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
expressionNameIdentifier :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.Identifier
expressionNameIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ExpressionName
expressionNameQualifier :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm (Maybe Syntax.AmbiguousName)
expressionNameQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.ExpressionName
expressionNameWithIdentifier :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ExpressionName
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
expressionNameWithQualifier :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm (Maybe Syntax.AmbiguousName) -> Typed.TypedTerm Syntax.ExpressionName
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
expressionStatement :: Typed.TypedTerm Syntax.StatementExpression -> Typed.TypedTerm Syntax.ExpressionStatement
expressionStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ExpressionStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.FieldAccess
fieldAccess :: Typed.TypedTerm Syntax.FieldAccess_Qualifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.FieldAccess
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
fieldAccessIdentifier :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Identifier
fieldAccessIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.FieldAccess
fieldAccessQualifier :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.FieldAccess_Qualifier
fieldAccessQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.FieldAccess
fieldAccessWithIdentifier :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.FieldAccess
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
fieldAccessWithQualifier :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.FieldAccess_Qualifier -> Typed.TypedTerm Syntax.FieldAccess
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
fieldAccess_QualifierPrimary :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierSuper :: Typed.TypedTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierSuper =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typed variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierTyped :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierTyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.FieldDeclaration
fieldDeclaration :: Typed.TypedTerm [Syntax.FieldModifier] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.FieldDeclaration
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
fieldDeclarationModifiers :: Typed.TypedTerm Syntax.FieldDeclaration -> Typed.TypedTerm [Syntax.FieldModifier]
fieldDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the unannType field of hydra.java.syntax.FieldDeclaration
fieldDeclarationUnannType :: Typed.TypedTerm Syntax.FieldDeclaration -> Typed.TypedTerm Syntax.UnannType
fieldDeclarationUnannType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionFieldName = (Core.Name "unannType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variableDeclarators field of hydra.java.syntax.FieldDeclaration
fieldDeclarationVariableDeclarators :: Typed.TypedTerm Syntax.FieldDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator]
fieldDeclarationVariableDeclarators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionFieldName = (Core.Name "variableDeclarators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.FieldDeclaration
fieldDeclarationWithModifiers :: Typed.TypedTerm Syntax.FieldDeclaration -> Typed.TypedTerm [Syntax.FieldModifier] -> Typed.TypedTerm Syntax.FieldDeclaration
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
fieldDeclarationWithUnannType :: Typed.TypedTerm Syntax.FieldDeclaration -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.FieldDeclaration
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
fieldDeclarationWithVariableDeclarators :: Typed.TypedTerm Syntax.FieldDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.FieldDeclaration
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
fieldModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.FieldModifier
fieldModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.FieldModifier
fieldModifierFinal :: Typed.TypedTerm Syntax.FieldModifier
fieldModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.FieldModifier
fieldModifierPrivate :: Typed.TypedTerm Syntax.FieldModifier
fieldModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.FieldModifier
fieldModifierProtected :: Typed.TypedTerm Syntax.FieldModifier
fieldModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.FieldModifier
fieldModifierPublic :: Typed.TypedTerm Syntax.FieldModifier
fieldModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.FieldModifier
fieldModifierStatic :: Typed.TypedTerm Syntax.FieldModifier
fieldModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transient variant of hydra.java.syntax.FieldModifier
fieldModifierTransient :: Typed.TypedTerm Syntax.FieldModifier
fieldModifierTransient =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transient"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the volatile variant of hydra.java.syntax.FieldModifier
fieldModifierVolatile :: Typed.TypedTerm Syntax.FieldModifier
fieldModifierVolatile =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "volatile"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.Finally wrapper
finally :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.Finally
finally x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Finally"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.FloatingPointLiteral wrapper
floatingPointLiteral :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.FloatingPointLiteral
floatingPointLiteral x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.FloatingPointLiteral"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the double variant of hydra.java.syntax.FloatingPointType
floatingPointTypeDouble :: Typed.TypedTerm Syntax.FloatingPointType
floatingPointTypeDouble =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float variant of hydra.java.syntax.FloatingPointType
floatingPointTypeFloat :: Typed.TypedTerm Syntax.FloatingPointType
floatingPointTypeFloat =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.ForCond
forCond :: Typed.TypedTerm (Maybe Syntax.ForInit) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.ForUpdate) -> Typed.TypedTerm Syntax.ForCond
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
forCondCond :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm (Maybe Syntax.Expression)
forCondCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.java.syntax.ForCond
forCondInit :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm (Maybe Syntax.ForInit)
forCondInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the update field of hydra.java.syntax.ForCond
forCondUpdate :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm (Maybe Syntax.ForUpdate)
forCondUpdate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionFieldName = (Core.Name "update")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ForCond
forCondWithCond :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ForCond
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
forCondWithInit :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm (Maybe Syntax.ForInit) -> Typed.TypedTerm Syntax.ForCond
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
forCondWithUpdate :: Typed.TypedTerm Syntax.ForCond -> Typed.TypedTerm (Maybe Syntax.ForUpdate) -> Typed.TypedTerm Syntax.ForCond
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
forInitLocalVariable :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm Syntax.ForInit
forInitLocalVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the statements variant of hydra.java.syntax.ForInit
forInitStatements :: Typed.TypedTerm [Syntax.StatementExpression] -> Typed.TypedTerm Syntax.ForInit
forInitStatements x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statements"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the basic variant of hydra.java.syntax.ForStatement
forStatementBasic :: Typed.TypedTerm Syntax.BasicForStatement -> Typed.TypedTerm Syntax.ForStatement
forStatementBasic x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the enhanced variant of hydra.java.syntax.ForStatement
forStatementEnhanced :: Typed.TypedTerm Syntax.EnhancedForStatement -> Typed.TypedTerm Syntax.ForStatement
forStatementEnhanced x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the basic variant of hydra.java.syntax.ForStatementNoShortIf
forStatementNoShortIfBasic :: Typed.TypedTerm Syntax.BasicForStatementNoShortIf -> Typed.TypedTerm Syntax.ForStatementNoShortIf
forStatementNoShortIfBasic x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the enhanced variant of hydra.java.syntax.ForStatementNoShortIf
forStatementNoShortIfEnhanced :: Typed.TypedTerm Syntax.EnhancedForStatementNoShortIf -> Typed.TypedTerm Syntax.ForStatementNoShortIf
forStatementNoShortIfEnhanced x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ForUpdate wrapper
forUpdate :: Typed.TypedTerm [Syntax.StatementExpression] -> Typed.TypedTerm Syntax.ForUpdate
forUpdate x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ForUpdate"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.FormalParameter
formalParameterSimple :: Typed.TypedTerm Syntax.FormalParameter_Simple -> Typed.TypedTerm Syntax.FormalParameter
formalParameterSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.FormalParameter
formalParameterVariableArity :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm Syntax.FormalParameter
formalParameterVariableArity x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.FormalParameter_Simple
formalParameter_Simple :: Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.FormalParameter_Simple
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
formalParameter_SimpleId :: Typed.TypedTerm Syntax.FormalParameter_Simple -> Typed.TypedTerm Syntax.VariableDeclaratorId
formalParameter_SimpleId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleModifiers :: Typed.TypedTerm Syntax.FormalParameter_Simple -> Typed.TypedTerm [Syntax.VariableModifier]
formalParameter_SimpleModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleType :: Typed.TypedTerm Syntax.FormalParameter_Simple -> Typed.TypedTerm Syntax.UnannType
formalParameter_SimpleType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleWithId :: Typed.TypedTerm Syntax.FormalParameter_Simple -> Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.FormalParameter_Simple
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
formalParameter_SimpleWithModifiers :: Typed.TypedTerm Syntax.FormalParameter_Simple -> Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.FormalParameter_Simple
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
formalParameter_SimpleWithType :: Typed.TypedTerm Syntax.FormalParameter_Simple -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.FormalParameter_Simple
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
guard :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Guard
guard x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Guard"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.Identifier wrapper
identifier :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Identifier
identifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Identifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.IfThenElseStatement
ifThenElseStatement :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.IfThenElseStatement
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
ifThenElseStatementCond :: Typed.TypedTerm Syntax.IfThenElseStatement -> Typed.TypedTerm (Maybe Syntax.Expression)
ifThenElseStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementElse :: Typed.TypedTerm Syntax.IfThenElseStatement -> Typed.TypedTerm Syntax.Statement
ifThenElseStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIf :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf
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
ifThenElseStatementNoShortIfCond :: Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf -> Typed.TypedTerm (Maybe Syntax.Expression)
ifThenElseStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfElse :: Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
ifThenElseStatementNoShortIfElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the then field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfThen :: Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
ifThenElseStatementNoShortIfThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithCond :: Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf
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
ifThenElseStatementNoShortIfWithElse :: Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf
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
ifThenElseStatementNoShortIfWithThen :: Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf
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
ifThenElseStatementThen :: Typed.TypedTerm Syntax.IfThenElseStatement -> Typed.TypedTerm Syntax.StatementNoShortIf
ifThenElseStatementThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementWithCond :: Typed.TypedTerm Syntax.IfThenElseStatement -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.IfThenElseStatement
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
ifThenElseStatementWithElse :: Typed.TypedTerm Syntax.IfThenElseStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.IfThenElseStatement
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
ifThenElseStatementWithThen :: Typed.TypedTerm Syntax.IfThenElseStatement -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.IfThenElseStatement
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
ifThenStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.IfThenStatement
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
ifThenStatementExpression :: Typed.TypedTerm Syntax.IfThenStatement -> Typed.TypedTerm Syntax.Expression
ifThenStatementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statement field of hydra.java.syntax.IfThenStatement
ifThenStatementStatement :: Typed.TypedTerm Syntax.IfThenStatement -> Typed.TypedTerm Syntax.Statement
ifThenStatementStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.IfThenStatement
ifThenStatementWithExpression :: Typed.TypedTerm Syntax.IfThenStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfThenStatement
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
ifThenStatementWithStatement :: Typed.TypedTerm Syntax.IfThenStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.IfThenStatement
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
importDeclarationSingleStaticImport :: Typed.TypedTerm Syntax.SingleStaticImportDeclaration -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationSingleStaticImport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleStaticImport"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the singleType variant of hydra.java.syntax.ImportDeclaration
importDeclarationSingleType :: Typed.TypedTerm Syntax.SingleTypeImportDeclaration -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationSingleType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the staticImportOnDemand variant of hydra.java.syntax.ImportDeclaration
importDeclarationStaticImportOnDemand :: Typed.TypedTerm Syntax.StaticImportOnDemandDeclaration -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationStaticImportOnDemand x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticImportOnDemand"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeImportOnDemand variant of hydra.java.syntax.ImportDeclaration
importDeclarationTypeImportOnDemand :: Typed.TypedTerm Syntax.TypeImportOnDemandDeclaration -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationTypeImportOnDemand x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeImportOnDemand"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.InclusiveOrExpression wrapper
inclusiveOrExpression :: Typed.TypedTerm [Syntax.ExclusiveOrExpression] -> Typed.TypedTerm Syntax.InclusiveOrExpression
inclusiveOrExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InclusiveOrExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.InstanceInitializer wrapper
instanceInitializer :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.InstanceInitializer
instanceInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InstanceInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.InstanceofExpression
instanceofExpression :: Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.InstanceofExpression_Rhs -> Typed.TypedTerm Syntax.InstanceofExpression
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
instanceofExpressionLhs :: Typed.TypedTerm Syntax.InstanceofExpression -> Typed.TypedTerm Syntax.RelationalExpression
instanceofExpressionLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionRhs :: Typed.TypedTerm Syntax.InstanceofExpression -> Typed.TypedTerm Syntax.InstanceofExpression_Rhs
instanceofExpressionRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionWithLhs :: Typed.TypedTerm Syntax.InstanceofExpression -> Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.InstanceofExpression
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
instanceofExpressionWithRhs :: Typed.TypedTerm Syntax.InstanceofExpression -> Typed.TypedTerm Syntax.InstanceofExpression_Rhs -> Typed.TypedTerm Syntax.InstanceofExpression
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
instanceofExpression_RhsPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.InstanceofExpression_Rhs
instanceofExpression_RhsPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression_Rhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the referenceType variant of hydra.java.syntax.InstanceofExpression_Rhs
instanceofExpression_RhsReferenceType :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.InstanceofExpression_Rhs
instanceofExpression_RhsReferenceType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression_Rhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "referenceType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.IntegerLiteral wrapper
integerLiteral :: Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.IntegerLiteral
integerLiteral x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.IntegerLiteral"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the byte variant of hydra.java.syntax.IntegralType
integralTypeByte :: Typed.TypedTerm Syntax.IntegralType
integralTypeByte =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the char variant of hydra.java.syntax.IntegralType
integralTypeChar :: Typed.TypedTerm Syntax.IntegralType
integralTypeChar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int variant of hydra.java.syntax.IntegralType
integralTypeInt :: Typed.TypedTerm Syntax.IntegralType
integralTypeInt =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the long variant of hydra.java.syntax.IntegralType
integralTypeLong :: Typed.TypedTerm Syntax.IntegralType
integralTypeLong =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the short variant of hydra.java.syntax.IntegralType
integralTypeShort :: Typed.TypedTerm Syntax.IntegralType
integralTypeShort =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.InterfaceBody wrapper
interfaceBody :: Typed.TypedTerm [Syntax.InterfaceMemberDeclarationWithComments] -> Typed.TypedTerm Syntax.InterfaceBody
interfaceBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InterfaceBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotationInterface variant of hydra.java.syntax.InterfaceDeclaration
interfaceDeclarationAnnotationInterface :: Typed.TypedTerm Syntax.AnnotationInterfaceDeclaration -> Typed.TypedTerm Syntax.InterfaceDeclaration
interfaceDeclarationAnnotationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normalInterface variant of hydra.java.syntax.InterfaceDeclaration
interfaceDeclarationNormalInterface :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm Syntax.InterfaceDeclaration
interfaceDeclarationNormalInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationClass :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constant variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationConstant :: Typed.TypedTerm Syntax.ConstantDeclaration -> Typed.TypedTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationConstant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterface :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interfaceMethod variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterfaceMethod :: Typed.TypedTerm Syntax.InterfaceMethodDeclaration -> Typed.TypedTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterfaceMethod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interfaceMethod"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithComments :: Typed.TypedTerm Syntax.InterfaceMemberDeclaration -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.InterfaceMemberDeclarationWithComments
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
interfaceMemberDeclarationWithCommentsComments :: Typed.TypedTerm Syntax.InterfaceMemberDeclarationWithComments -> Typed.TypedTerm (Maybe String)
interfaceMemberDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsValue :: Typed.TypedTerm Syntax.InterfaceMemberDeclarationWithComments -> Typed.TypedTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationWithCommentsValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsWithComments :: Typed.TypedTerm Syntax.InterfaceMemberDeclarationWithComments -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.InterfaceMemberDeclarationWithComments
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
interfaceMemberDeclarationWithCommentsWithValue :: Typed.TypedTerm Syntax.InterfaceMemberDeclarationWithComments -> Typed.TypedTerm Syntax.InterfaceMemberDeclaration -> Typed.TypedTerm Syntax.InterfaceMemberDeclarationWithComments
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
interfaceMethodDeclaration :: Typed.TypedTerm [Syntax.InterfaceMethodModifier] -> Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.MethodBody -> Typed.TypedTerm Syntax.InterfaceMethodDeclaration
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
interfaceMethodDeclarationBody :: Typed.TypedTerm Syntax.InterfaceMethodDeclaration -> Typed.TypedTerm Syntax.MethodBody
interfaceMethodDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationHeader :: Typed.TypedTerm Syntax.InterfaceMethodDeclaration -> Typed.TypedTerm Syntax.MethodHeader
interfaceMethodDeclarationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationModifiers :: Typed.TypedTerm Syntax.InterfaceMethodDeclaration -> Typed.TypedTerm [Syntax.InterfaceMethodModifier]
interfaceMethodDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithBody :: Typed.TypedTerm Syntax.InterfaceMethodDeclaration -> Typed.TypedTerm Syntax.MethodBody -> Typed.TypedTerm Syntax.InterfaceMethodDeclaration
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
interfaceMethodDeclarationWithHeader :: Typed.TypedTerm Syntax.InterfaceMethodDeclaration -> Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.InterfaceMethodDeclaration
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
interfaceMethodDeclarationWithModifiers :: Typed.TypedTerm Syntax.InterfaceMethodDeclaration -> Typed.TypedTerm [Syntax.InterfaceMethodModifier] -> Typed.TypedTerm Syntax.InterfaceMethodDeclaration
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
interfaceMethodModifierAbstract :: Typed.TypedTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the default variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierDefault :: Typed.TypedTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierDefault =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierPrivate :: Typed.TypedTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierPublic :: Typed.TypedTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierStatic :: Typed.TypedTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierStrictfp :: Typed.TypedTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.InterfaceModifier
interfaceModifierAbstract :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.InterfaceModifier
interfaceModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nonSealed variant of hydra.java.syntax.InterfaceModifier
interfaceModifierNonSealed :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierNonSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonSealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.InterfaceModifier
interfaceModifierPrivate :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.InterfaceModifier
interfaceModifierProtected :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.InterfaceModifier
interfaceModifierPublic :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sealed variant of hydra.java.syntax.InterfaceModifier
interfaceModifierSealed :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.InterfaceModifier
interfaceModifierStatic :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.InterfaceModifier
interfaceModifierStrictfp :: Typed.TypedTerm Syntax.InterfaceModifier
interfaceModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.InterfaceType wrapper
interfaceType :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.InterfaceType
interfaceType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InterfaceType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.LabeledStatement
labeledStatement :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.LabeledStatement
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
labeledStatementIdentifier :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Identifier
labeledStatementIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIf :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.LabeledStatementNoShortIf
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
labeledStatementNoShortIfIdentifier :: Typed.TypedTerm Syntax.LabeledStatementNoShortIf -> Typed.TypedTerm Syntax.Identifier
labeledStatementNoShortIfIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statement field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfStatement :: Typed.TypedTerm Syntax.LabeledStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
labeledStatementNoShortIfStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithIdentifier :: Typed.TypedTerm Syntax.LabeledStatementNoShortIf -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.LabeledStatementNoShortIf
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
labeledStatementNoShortIfWithStatement :: Typed.TypedTerm Syntax.LabeledStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.LabeledStatementNoShortIf
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
labeledStatementStatement :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Statement
labeledStatementStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.LabeledStatement
labeledStatementWithIdentifier :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.LabeledStatement
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
labeledStatementWithStatement :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.LabeledStatement
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
lambdaBodyBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.LambdaBody
lambdaBodyBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.LambdaBody
lambdaBodyExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.LambdaBody
lambdaBodyExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LambdaExpression
lambdaExpression :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm Syntax.LambdaBody -> Typed.TypedTerm Syntax.LambdaExpression
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
lambdaExpressionBody :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.LambdaBody
lambdaExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.LambdaExpression
lambdaExpressionParameters :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.LambdaParameters
lambdaExpressionParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.LambdaExpression
lambdaExpressionWithBody :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.LambdaBody -> Typed.TypedTerm Syntax.LambdaExpression
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
lambdaExpressionWithParameters :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm Syntax.LambdaExpression
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
lambdaParameterNormal :: Typed.TypedTerm Syntax.LambdaParameter_Normal -> Typed.TypedTerm Syntax.LambdaParameter
lambdaParameterNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.LambdaParameterType
lambdaParameterTypeType :: Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.LambdaParameterType
lambdaParameterTypeType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.java.syntax.LambdaParameterType
lambdaParameterTypeVar :: Typed.TypedTerm Syntax.LambdaParameterType
lambdaParameterTypeVar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.LambdaParameter
lambdaParameterVariableArity :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm Syntax.LambdaParameter
lambdaParameterVariableArity x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_Normal :: Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.LambdaParameterType -> Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.LambdaParameter_Normal
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
lambdaParameter_NormalId :: Typed.TypedTerm Syntax.LambdaParameter_Normal -> Typed.TypedTerm Syntax.VariableDeclaratorId
lambdaParameter_NormalId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalModifiers :: Typed.TypedTerm Syntax.LambdaParameter_Normal -> Typed.TypedTerm [Syntax.VariableModifier]
lambdaParameter_NormalModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalType :: Typed.TypedTerm Syntax.LambdaParameter_Normal -> Typed.TypedTerm Syntax.LambdaParameterType
lambdaParameter_NormalType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalWithId :: Typed.TypedTerm Syntax.LambdaParameter_Normal -> Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.LambdaParameter_Normal
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
lambdaParameter_NormalWithModifiers :: Typed.TypedTerm Syntax.LambdaParameter_Normal -> Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.LambdaParameter_Normal
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
lambdaParameter_NormalWithType :: Typed.TypedTerm Syntax.LambdaParameter_Normal -> Typed.TypedTerm Syntax.LambdaParameterType -> Typed.TypedTerm Syntax.LambdaParameter_Normal
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
lambdaParametersSingle :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParametersSingle x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.java.syntax.LambdaParameters
lambdaParametersTuple :: Typed.TypedTerm [Syntax.LambdaParameters] -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParametersTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the arrayAccess variant of hydra.java.syntax.LeftHandSide
leftHandSideArrayAccess :: Typed.TypedTerm Syntax.ArrayAccess -> Typed.TypedTerm Syntax.LeftHandSide
leftHandSideArrayAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expressionName variant of hydra.java.syntax.LeftHandSide
leftHandSideExpressionName :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.LeftHandSide
leftHandSideExpressionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.LeftHandSide
leftHandSideFieldAccess :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.LeftHandSide
leftHandSideFieldAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.Literal
literalBoolean :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Literal
literalBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the character variant of hydra.java.syntax.Literal
literalCharacter :: Typed.TypedTerm Int -> Typed.TypedTerm Syntax.Literal
literalCharacter x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the floatingPoint variant of hydra.java.syntax.Literal
literalFloatingPoint :: Typed.TypedTerm Syntax.FloatingPointLiteral -> Typed.TypedTerm Syntax.Literal
literalFloatingPoint x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.java.syntax.Literal
literalInteger :: Typed.TypedTerm Syntax.IntegerLiteral -> Typed.TypedTerm Syntax.Literal
literalInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the null variant of hydra.java.syntax.Literal
literalNull :: Typed.TypedTerm Syntax.Literal
literalNull =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the string variant of hydra.java.syntax.Literal
literalString :: Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Syntax.Literal
literalString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the textBlock variant of hydra.java.syntax.Literal
literalTextBlock :: Typed.TypedTerm Syntax.TextBlock -> Typed.TypedTerm Syntax.Literal
literalTextBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "textBlock"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationClass :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normalInterface variant of hydra.java.syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationNormalInterface :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm Syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationNormalInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LocalVariableDeclaration
localVariableDeclaration :: Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.LocalVariableType -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.LocalVariableDeclaration
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
localVariableDeclarationDeclarators :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator]
localVariableDeclarationDeclarators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionFieldName = (Core.Name "declarators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationModifiers :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm [Syntax.VariableModifier]
localVariableDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.LocalVariableDeclarationStatement wrapper
localVariableDeclarationStatement :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm Syntax.LocalVariableDeclarationStatement
localVariableDeclarationStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclarationStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationType :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm Syntax.LocalVariableType
localVariableDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarators field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationWithDeclarators :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.LocalVariableDeclaration
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
localVariableDeclarationWithModifiers :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.LocalVariableDeclaration
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
localVariableDeclarationWithType :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm Syntax.LocalVariableType -> Typed.TypedTerm Syntax.LocalVariableDeclaration
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
localVariableTypeType :: Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.LocalVariableType
localVariableTypeType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.java.syntax.LocalVariableType
localVariableTypeVar :: Typed.TypedTerm Syntax.LocalVariableType
localVariableTypeVar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.MarkerAnnotation wrapper
markerAnnotation :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.MarkerAnnotation
markerAnnotation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MarkerAnnotation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the block variant of hydra.java.syntax.MethodBody
methodBodyBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.MethodBody
methodBodyBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.MethodBody
methodBodyNone :: Typed.TypedTerm Syntax.MethodBody
methodBodyNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.MethodDeclaration
methodDeclaration :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm [Syntax.MethodModifier] -> Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.MethodBody -> Typed.TypedTerm Syntax.MethodDeclaration
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
methodDeclarationAnnotations :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm [Syntax.Annotation]
methodDeclarationAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.MethodDeclaration
methodDeclarationBody :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm Syntax.MethodBody
methodDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.MethodDeclaration
methodDeclarationHeader :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm Syntax.MethodHeader
methodDeclarationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.MethodDeclaration
methodDeclarationModifiers :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm [Syntax.MethodModifier]
methodDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithAnnotations :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.MethodDeclaration
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
methodDeclarationWithBody :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm Syntax.MethodBody -> Typed.TypedTerm Syntax.MethodDeclaration
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
methodDeclarationWithHeader :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.MethodDeclaration
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
methodDeclarationWithModifiers :: Typed.TypedTerm Syntax.MethodDeclaration -> Typed.TypedTerm [Syntax.MethodModifier] -> Typed.TypedTerm Syntax.MethodDeclaration
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
methodDeclarator :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.ReceiverParameter) -> Typed.TypedTerm [Syntax.FormalParameter] -> Typed.TypedTerm Syntax.MethodDeclarator
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
methodDeclaratorFormalParameters :: Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm [Syntax.FormalParameter]
methodDeclaratorFormalParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionFieldName = (Core.Name "formalParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodDeclarator
methodDeclaratorIdentifier :: Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm Syntax.Identifier
methodDeclaratorIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the receiverParameter field of hydra.java.syntax.MethodDeclarator
methodDeclaratorReceiverParameter :: Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm (Maybe Syntax.ReceiverParameter)
methodDeclaratorReceiverParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionFieldName = (Core.Name "receiverParameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the formalParameters field of hydra.java.syntax.MethodDeclarator
methodDeclaratorWithFormalParameters :: Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm [Syntax.FormalParameter] -> Typed.TypedTerm Syntax.MethodDeclarator
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
methodDeclaratorWithIdentifier :: Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodDeclarator
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
methodDeclaratorWithReceiverParameter :: Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm (Maybe Syntax.ReceiverParameter) -> Typed.TypedTerm Syntax.MethodDeclarator
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
methodHeader :: Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.Result -> Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm (Maybe Syntax.Throws) -> Typed.TypedTerm Syntax.MethodHeader
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
methodHeaderDeclarator :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.MethodDeclarator
methodHeaderDeclarator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "declarator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.MethodHeader
methodHeaderParameters :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm [Syntax.TypeParameter]
methodHeaderParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the result field of hydra.java.syntax.MethodHeader
methodHeaderResult :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.Result
methodHeaderResult x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "result")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the throws field of hydra.java.syntax.MethodHeader
methodHeaderThrows :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm (Maybe Syntax.Throws)
methodHeaderThrows x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionFieldName = (Core.Name "throws")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarator field of hydra.java.syntax.MethodHeader
methodHeaderWithDeclarator :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.MethodDeclarator -> Typed.TypedTerm Syntax.MethodHeader
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
methodHeaderWithParameters :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.MethodHeader
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
methodHeaderWithResult :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm Syntax.Result -> Typed.TypedTerm Syntax.MethodHeader
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
methodHeaderWithThrows :: Typed.TypedTerm Syntax.MethodHeader -> Typed.TypedTerm (Maybe Syntax.Throws) -> Typed.TypedTerm Syntax.MethodHeader
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
methodInvocation :: Typed.TypedTerm Syntax.MethodInvocation_Header -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.MethodInvocation
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
methodInvocationArguments :: Typed.TypedTerm Syntax.MethodInvocation -> Typed.TypedTerm [Syntax.Expression]
methodInvocationArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.MethodInvocation
methodInvocationHeader :: Typed.TypedTerm Syntax.MethodInvocation -> Typed.TypedTerm Syntax.MethodInvocation_Header
methodInvocationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.MethodInvocation
methodInvocationWithArguments :: Typed.TypedTerm Syntax.MethodInvocation -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.MethodInvocation
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
methodInvocationWithHeader :: Typed.TypedTerm Syntax.MethodInvocation -> Typed.TypedTerm Syntax.MethodInvocation_Header -> Typed.TypedTerm Syntax.MethodInvocation
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
methodInvocation_Complex :: Typed.TypedTerm Syntax.MethodInvocation_Variant -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodInvocation_Complex
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
methodInvocation_ComplexIdentifier :: Typed.TypedTerm Syntax.MethodInvocation_Complex -> Typed.TypedTerm Syntax.Identifier
methodInvocation_ComplexIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexTypeArguments :: Typed.TypedTerm Syntax.MethodInvocation_Complex -> Typed.TypedTerm [Syntax.TypeArgument]
methodInvocation_ComplexTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexVariant :: Typed.TypedTerm Syntax.MethodInvocation_Complex -> Typed.TypedTerm Syntax.MethodInvocation_Variant
methodInvocation_ComplexVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexWithIdentifier :: Typed.TypedTerm Syntax.MethodInvocation_Complex -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodInvocation_Complex
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
methodInvocation_ComplexWithTypeArguments :: Typed.TypedTerm Syntax.MethodInvocation_Complex -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.MethodInvocation_Complex
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
methodInvocation_ComplexWithVariant :: Typed.TypedTerm Syntax.MethodInvocation_Complex -> Typed.TypedTerm Syntax.MethodInvocation_Variant -> Typed.TypedTerm Syntax.MethodInvocation_Complex
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
methodInvocation_HeaderComplex :: Typed.TypedTerm Syntax.MethodInvocation_Complex -> Typed.TypedTerm Syntax.MethodInvocation_Header
methodInvocation_HeaderComplex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.MethodInvocation_Header
methodInvocation_HeaderSimple :: Typed.TypedTerm Syntax.MethodName -> Typed.TypedTerm Syntax.MethodInvocation_Header
methodInvocation_HeaderSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantExpression :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantPrimary :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantSuper :: Typed.TypedTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantSuper =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantType :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeSuper variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantTypeSuper :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantTypeSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSuper"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.MethodModifier
methodModifierAbstract :: Typed.TypedTerm Syntax.MethodModifier
methodModifierAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.MethodModifier
methodModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.MethodModifier
methodModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.MethodModifier
methodModifierFinal :: Typed.TypedTerm Syntax.MethodModifier
methodModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the native variant of hydra.java.syntax.MethodModifier
methodModifierNative :: Typed.TypedTerm Syntax.MethodModifier
methodModifierNative =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "native"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.MethodModifier
methodModifierPrivate :: Typed.TypedTerm Syntax.MethodModifier
methodModifierPrivate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.MethodModifier
methodModifierProtected :: Typed.TypedTerm Syntax.MethodModifier
methodModifierProtected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.MethodModifier
methodModifierPublic :: Typed.TypedTerm Syntax.MethodModifier
methodModifierPublic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.MethodModifier
methodModifierStatic :: Typed.TypedTerm Syntax.MethodModifier
methodModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.MethodModifier
methodModifierStrictfp :: Typed.TypedTerm Syntax.MethodModifier
methodModifierStrictfp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the synchronized variant of hydra.java.syntax.MethodModifier
methodModifierSynchronized :: Typed.TypedTerm Syntax.MethodModifier
methodModifierSynchronized =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.MethodName wrapper
methodName :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodName
methodName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MethodName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the array variant of hydra.java.syntax.MethodReference
methodReferenceArray :: Typed.TypedTerm Syntax.MethodReference_Array -> Typed.TypedTerm Syntax.MethodReference
methodReferenceArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.MethodReference
methodReferenceExpression :: Typed.TypedTerm Syntax.MethodReference_Expression -> Typed.TypedTerm Syntax.MethodReference
methodReferenceExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the new variant of hydra.java.syntax.MethodReference
methodReferenceNew :: Typed.TypedTerm Syntax.MethodReference_New -> Typed.TypedTerm Syntax.MethodReference
methodReferenceNew x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.MethodReference
methodReferencePrimary :: Typed.TypedTerm Syntax.MethodReference_Primary -> Typed.TypedTerm Syntax.MethodReference
methodReferencePrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the referenceType variant of hydra.java.syntax.MethodReference
methodReferenceReferenceType :: Typed.TypedTerm Syntax.MethodReference_ReferenceType -> Typed.TypedTerm Syntax.MethodReference
methodReferenceReferenceType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "referenceType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.MethodReference
methodReferenceSuper :: Typed.TypedTerm Syntax.MethodReference_Super -> Typed.TypedTerm Syntax.MethodReference
methodReferenceSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.MethodReference_Array wrapper
methodReference_Array :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.MethodReference_Array
methodReference_Array x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MethodReference_Array"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.MethodReference_Expression
methodReference_Expression :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodReference_Expression
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
methodReference_ExpressionIdentifier :: Typed.TypedTerm Syntax.MethodReference_Expression -> Typed.TypedTerm Syntax.Identifier
methodReference_ExpressionIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionName :: Typed.TypedTerm Syntax.MethodReference_Expression -> Typed.TypedTerm Syntax.ExpressionName
methodReference_ExpressionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionTypeArguments :: Typed.TypedTerm Syntax.MethodReference_Expression -> Typed.TypedTerm [Syntax.TypeArgument]
methodReference_ExpressionTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionWithIdentifier :: Typed.TypedTerm Syntax.MethodReference_Expression -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodReference_Expression
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
methodReference_ExpressionWithName :: Typed.TypedTerm Syntax.MethodReference_Expression -> Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.MethodReference_Expression
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
methodReference_ExpressionWithTypeArguments :: Typed.TypedTerm Syntax.MethodReference_Expression -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.MethodReference_Expression
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
methodReference_New :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.MethodReference_New
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
methodReference_NewClassType :: Typed.TypedTerm Syntax.MethodReference_New -> Typed.TypedTerm Syntax.ClassType
methodReference_NewClassType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
        Core.projectionFieldName = (Core.Name "classType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_New
methodReference_NewTypeArguments :: Typed.TypedTerm Syntax.MethodReference_New -> Typed.TypedTerm [Syntax.TypeArgument]
methodReference_NewTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the classType field of hydra.java.syntax.MethodReference_New
methodReference_NewWithClassType :: Typed.TypedTerm Syntax.MethodReference_New -> Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.MethodReference_New
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
methodReference_NewWithTypeArguments :: Typed.TypedTerm Syntax.MethodReference_New -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.MethodReference_New
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
methodReference_Primary :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodReference_Primary
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
methodReference_PrimaryIdentifier :: Typed.TypedTerm Syntax.MethodReference_Primary -> Typed.TypedTerm Syntax.Identifier
methodReference_PrimaryIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryPrimary :: Typed.TypedTerm Syntax.MethodReference_Primary -> Typed.TypedTerm Syntax.Primary
methodReference_PrimaryPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryTypeArguments :: Typed.TypedTerm Syntax.MethodReference_Primary -> Typed.TypedTerm [Syntax.TypeArgument]
methodReference_PrimaryTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryWithIdentifier :: Typed.TypedTerm Syntax.MethodReference_Primary -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodReference_Primary
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
methodReference_PrimaryWithPrimary :: Typed.TypedTerm Syntax.MethodReference_Primary -> Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.MethodReference_Primary
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
methodReference_PrimaryWithTypeArguments :: Typed.TypedTerm Syntax.MethodReference_Primary -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.MethodReference_Primary
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
methodReference_ReferenceType :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodReference_ReferenceType
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
methodReference_ReferenceTypeIdentifier :: Typed.TypedTerm Syntax.MethodReference_ReferenceType -> Typed.TypedTerm Syntax.Identifier
methodReference_ReferenceTypeIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the referenceType field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeReferenceType :: Typed.TypedTerm Syntax.MethodReference_ReferenceType -> Typed.TypedTerm Syntax.ReferenceType
methodReference_ReferenceTypeReferenceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionFieldName = (Core.Name "referenceType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeTypeArguments :: Typed.TypedTerm Syntax.MethodReference_ReferenceType -> Typed.TypedTerm [Syntax.TypeArgument]
methodReference_ReferenceTypeTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithIdentifier :: Typed.TypedTerm Syntax.MethodReference_ReferenceType -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodReference_ReferenceType
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
methodReference_ReferenceTypeWithReferenceType :: Typed.TypedTerm Syntax.MethodReference_ReferenceType -> Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.MethodReference_ReferenceType
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
methodReference_ReferenceTypeWithTypeArguments :: Typed.TypedTerm Syntax.MethodReference_ReferenceType -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.MethodReference_ReferenceType
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
methodReference_Super :: Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MethodReference_Super
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
methodReference_SuperIdentifier :: Typed.TypedTerm Syntax.MethodReference_Super -> Typed.TypedTerm Syntax.Identifier
methodReference_SuperIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the super field of hydra.java.syntax.MethodReference_Super
methodReference_SuperSuper :: Typed.TypedTerm Syntax.MethodReference_Super -> Typed.TypedTerm Bool
methodReference_SuperSuper x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionFieldName = (Core.Name "super")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Super
methodReference_SuperTypeArguments :: Typed.TypedTerm Syntax.MethodReference_Super -> Typed.TypedTerm [Syntax.TypeArgument]
methodReference_SuperTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Super
methodReference_SuperWithIdentifier :: Typed.TypedTerm Syntax.MethodReference_Super -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodReference_Super
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
methodReference_SuperWithSuper :: Typed.TypedTerm Syntax.MethodReference_Super -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MethodReference_Super
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
methodReference_SuperWithTypeArguments :: Typed.TypedTerm Syntax.MethodReference_Super -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.MethodReference_Super
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
modularCompilationUnit :: Typed.TypedTerm [Syntax.ImportDeclaration] -> Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm Syntax.ModularCompilationUnit
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
modularCompilationUnitImports :: Typed.TypedTerm Syntax.ModularCompilationUnit -> Typed.TypedTerm [Syntax.ImportDeclaration]
modularCompilationUnitImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the module field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitModule :: Typed.TypedTerm Syntax.ModularCompilationUnit -> Typed.TypedTerm Syntax.ModuleDeclaration
modularCompilationUnitModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the imports field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitWithImports :: Typed.TypedTerm Syntax.ModularCompilationUnit -> Typed.TypedTerm [Syntax.ImportDeclaration] -> Typed.TypedTerm Syntax.ModularCompilationUnit
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
modularCompilationUnitWithModule :: Typed.TypedTerm Syntax.ModularCompilationUnit -> Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm Syntax.ModularCompilationUnit
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
moduleDeclaration :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Bool -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm [Syntax.ModuleDirective] -> Typed.TypedTerm Syntax.ModuleDeclaration
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
moduleDeclarationAnnotations :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm [Syntax.Annotation]
moduleDeclarationAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the directives field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationDirectives :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm [Syntax.ModuleDirective]
moduleDeclarationDirectives x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "directives")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifiers field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationIdentifiers :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm [Syntax.Identifier]
moduleDeclarationIdentifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "identifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the open field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationOpen :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm Bool
moduleDeclarationOpen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "open")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithAnnotations :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.ModuleDeclaration
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
moduleDeclarationWithDirectives :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm [Syntax.ModuleDirective] -> Typed.TypedTerm Syntax.ModuleDeclaration
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
moduleDeclarationWithIdentifiers :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.ModuleDeclaration
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
moduleDeclarationWithOpen :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ModuleDeclaration
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
moduleDirectiveExports :: Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens -> Typed.TypedTerm Syntax.ModuleDirective
moduleDirectiveExports x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exports"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the opens variant of hydra.java.syntax.ModuleDirective
moduleDirectiveOpens :: Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens -> Typed.TypedTerm Syntax.ModuleDirective
moduleDirectiveOpens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the provides variant of hydra.java.syntax.ModuleDirective
moduleDirectiveProvides :: Typed.TypedTerm Syntax.ModuleDirective_Provides -> Typed.TypedTerm Syntax.ModuleDirective
moduleDirectiveProvides x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "provides"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the requires variant of hydra.java.syntax.ModuleDirective
moduleDirectiveRequires :: Typed.TypedTerm Syntax.ModuleDirective_Requires -> Typed.TypedTerm Syntax.ModuleDirective
moduleDirectiveRequires x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "requires"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the uses variant of hydra.java.syntax.ModuleDirective
moduleDirectiveUses :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.ModuleDirective
moduleDirectiveUses x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uses"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpens :: Typed.TypedTerm Syntax.PackageName -> Typed.TypedTerm [Syntax.ModuleName] -> Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens
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
moduleDirective_ExportsOrOpensModules :: Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens -> Typed.TypedTerm [Syntax.ModuleName]
moduleDirective_ExportsOrOpensModules x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionFieldName = (Core.Name "modules")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensPackage :: Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens -> Typed.TypedTerm Syntax.PackageName
moduleDirective_ExportsOrOpensPackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modules field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithModules :: Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens -> Typed.TypedTerm [Syntax.ModuleName] -> Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens
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
moduleDirective_ExportsOrOpensWithPackage :: Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens -> Typed.TypedTerm Syntax.PackageName -> Typed.TypedTerm Syntax.ModuleDirective_ExportsOrOpens
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
moduleDirective_Provides :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm [Syntax.TypeName] -> Typed.TypedTerm Syntax.ModuleDirective_Provides
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
moduleDirective_ProvidesTo :: Typed.TypedTerm Syntax.ModuleDirective_Provides -> Typed.TypedTerm Syntax.TypeName
moduleDirective_ProvidesTo x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
        Core.projectionFieldName = (Core.Name "to")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the with field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWith :: Typed.TypedTerm Syntax.ModuleDirective_Provides -> Typed.TypedTerm [Syntax.TypeName]
moduleDirective_ProvidesWith x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the to field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithTo :: Typed.TypedTerm Syntax.ModuleDirective_Provides -> Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.ModuleDirective_Provides
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
moduleDirective_ProvidesWithWith :: Typed.TypedTerm Syntax.ModuleDirective_Provides -> Typed.TypedTerm [Syntax.TypeName] -> Typed.TypedTerm Syntax.ModuleDirective_Provides
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
moduleDirective_Requires :: Typed.TypedTerm [Syntax.RequiresModifier] -> Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm Syntax.ModuleDirective_Requires
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
moduleDirective_RequiresModifiers :: Typed.TypedTerm Syntax.ModuleDirective_Requires -> Typed.TypedTerm [Syntax.RequiresModifier]
moduleDirective_RequiresModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the module field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresModule :: Typed.TypedTerm Syntax.ModuleDirective_Requires -> Typed.TypedTerm Syntax.ModuleName
moduleDirective_RequiresModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModifiers :: Typed.TypedTerm Syntax.ModuleDirective_Requires -> Typed.TypedTerm [Syntax.RequiresModifier] -> Typed.TypedTerm Syntax.ModuleDirective_Requires
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
moduleDirective_RequiresWithModule :: Typed.TypedTerm Syntax.ModuleDirective_Requires -> Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm Syntax.ModuleDirective_Requires
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
moduleName :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.ModuleName) -> Typed.TypedTerm Syntax.ModuleName
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
moduleNameIdentifier :: Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm Syntax.Identifier
moduleNameIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.ModuleName
moduleNameName :: Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm (Maybe Syntax.ModuleName)
moduleNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.ModuleName
moduleNameWithIdentifier :: Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ModuleName
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
moduleNameWithName :: Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm (Maybe Syntax.ModuleName) -> Typed.TypedTerm Syntax.ModuleName
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
multiplicativeExpressionDivide :: Typed.TypedTerm Syntax.MultiplicativeExpression_Binary -> Typed.TypedTerm Syntax.MultiplicativeExpression
multiplicativeExpressionDivide x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mod variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionMod :: Typed.TypedTerm Syntax.MultiplicativeExpression_Binary -> Typed.TypedTerm Syntax.MultiplicativeExpression
multiplicativeExpressionMod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the times variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionTimes :: Typed.TypedTerm Syntax.MultiplicativeExpression_Binary -> Typed.TypedTerm Syntax.MultiplicativeExpression
multiplicativeExpressionTimes x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionUnary :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.MultiplicativeExpression
multiplicativeExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_Binary :: Typed.TypedTerm Syntax.MultiplicativeExpression -> Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.MultiplicativeExpression_Binary
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
multiplicativeExpression_BinaryLhs :: Typed.TypedTerm Syntax.MultiplicativeExpression_Binary -> Typed.TypedTerm Syntax.MultiplicativeExpression
multiplicativeExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryRhs :: Typed.TypedTerm Syntax.MultiplicativeExpression_Binary -> Typed.TypedTerm Syntax.UnaryExpression
multiplicativeExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithLhs :: Typed.TypedTerm Syntax.MultiplicativeExpression_Binary -> Typed.TypedTerm Syntax.MultiplicativeExpression -> Typed.TypedTerm Syntax.MultiplicativeExpression_Binary
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
multiplicativeExpression_BinaryWithRhs :: Typed.TypedTerm Syntax.MultiplicativeExpression_Binary -> Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.MultiplicativeExpression_Binary
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
normalAnnotation :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm [Syntax.ElementValuePair] -> Typed.TypedTerm Syntax.NormalAnnotation
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
normalAnnotationPairs :: Typed.TypedTerm Syntax.NormalAnnotation -> Typed.TypedTerm [Syntax.ElementValuePair]
normalAnnotationPairs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
        Core.projectionFieldName = (Core.Name "pairs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.java.syntax.NormalAnnotation
normalAnnotationTypeName :: Typed.TypedTerm Syntax.NormalAnnotation -> Typed.TypedTerm Syntax.TypeName
normalAnnotationTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pairs field of hydra.java.syntax.NormalAnnotation
normalAnnotationWithPairs :: Typed.TypedTerm Syntax.NormalAnnotation -> Typed.TypedTerm [Syntax.ElementValuePair] -> Typed.TypedTerm Syntax.NormalAnnotation
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
normalAnnotationWithTypeName :: Typed.TypedTerm Syntax.NormalAnnotation -> Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.NormalAnnotation
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
normalClassDeclaration :: Typed.TypedTerm [Syntax.ClassModifier] -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm (Maybe Syntax.ClassType) -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm [Syntax.TypeName] -> Typed.TypedTerm Syntax.ClassBody -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalClassDeclarationBody :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm Syntax.ClassBody
normalClassDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the extends field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationExtends :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm (Maybe Syntax.ClassType)
normalClassDeclarationExtends x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "extends")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationIdentifier :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier
normalClassDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationImplements :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.InterfaceType]
normalClassDeclarationImplements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "implements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationModifiers :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.ClassModifier]
normalClassDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationParameters :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.TypeParameter]
normalClassDeclarationParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the permits field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationPermits :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.TypeName]
normalClassDeclarationPermits x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionFieldName = (Core.Name "permits")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithBody :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm Syntax.ClassBody -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalClassDeclarationWithExtends :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm (Maybe Syntax.ClassType) -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalClassDeclarationWithIdentifier :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalClassDeclarationWithImplements :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalClassDeclarationWithModifiers :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.ClassModifier] -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalClassDeclarationWithParameters :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalClassDeclarationWithPermits :: Typed.TypedTerm Syntax.NormalClassDeclaration -> Typed.TypedTerm [Syntax.TypeName] -> Typed.TypedTerm Syntax.NormalClassDeclaration
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
normalInterfaceDeclaration :: Typed.TypedTerm [Syntax.InterfaceModifier] -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm [Syntax.TypeName] -> Typed.TypedTerm Syntax.InterfaceBody -> Typed.TypedTerm Syntax.NormalInterfaceDeclaration
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
normalInterfaceDeclarationBody :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm Syntax.InterfaceBody
normalInterfaceDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the extends field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationExtends :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.InterfaceType]
normalInterfaceDeclarationExtends x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "extends")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationIdentifier :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier
normalInterfaceDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationModifiers :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.InterfaceModifier]
normalInterfaceDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationParameters :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeParameter]
normalInterfaceDeclarationParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the permits field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationPermits :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeName]
normalInterfaceDeclarationPermits x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "permits")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithBody :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm Syntax.InterfaceBody -> Typed.TypedTerm Syntax.NormalInterfaceDeclaration
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
normalInterfaceDeclarationWithExtends :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm Syntax.NormalInterfaceDeclaration
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
normalInterfaceDeclarationWithIdentifier :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.NormalInterfaceDeclaration
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
normalInterfaceDeclarationWithModifiers :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.InterfaceModifier] -> Typed.TypedTerm Syntax.NormalInterfaceDeclaration
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
normalInterfaceDeclarationWithParameters :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.NormalInterfaceDeclaration
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
normalInterfaceDeclarationWithPermits :: Typed.TypedTerm Syntax.NormalInterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeName] -> Typed.TypedTerm Syntax.NormalInterfaceDeclaration
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
numericTypeArrayArray :: Typed.TypedTerm Syntax.NumericTypeArray -> Typed.TypedTerm Syntax.NumericTypeArray
numericTypeArrayArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.NumericTypeArray
numericTypeArraySimple :: Typed.TypedTerm Syntax.NumericType -> Typed.TypedTerm Syntax.NumericTypeArray
numericTypeArraySimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the floatingPoint variant of hydra.java.syntax.NumericType
numericTypeFloatingPoint :: Typed.TypedTerm Syntax.FloatingPointType -> Typed.TypedTerm Syntax.NumericType
numericTypeFloatingPoint x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integral variant of hydra.java.syntax.NumericType
numericTypeIntegral :: Typed.TypedTerm Syntax.IntegralType -> Typed.TypedTerm Syntax.NumericType
numericTypeIntegral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integral"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnit :: Typed.TypedTerm (Maybe Syntax.PackageDeclaration) -> Typed.TypedTerm [Syntax.ImportDeclaration] -> Typed.TypedTerm [Syntax.TopLevelClassOrInterfaceDeclarationWithComments] -> Typed.TypedTerm Syntax.OrdinaryCompilationUnit
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
ordinaryCompilationUnitImports :: Typed.TypedTerm Syntax.OrdinaryCompilationUnit -> Typed.TypedTerm [Syntax.ImportDeclaration]
ordinaryCompilationUnitImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitPackage :: Typed.TypedTerm Syntax.OrdinaryCompilationUnit -> Typed.TypedTerm (Maybe Syntax.PackageDeclaration)
ordinaryCompilationUnitPackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the types field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitTypes :: Typed.TypedTerm Syntax.OrdinaryCompilationUnit -> Typed.TypedTerm [Syntax.TopLevelClassOrInterfaceDeclarationWithComments]
ordinaryCompilationUnitTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the imports field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithImports :: Typed.TypedTerm Syntax.OrdinaryCompilationUnit -> Typed.TypedTerm [Syntax.ImportDeclaration] -> Typed.TypedTerm Syntax.OrdinaryCompilationUnit
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
ordinaryCompilationUnitWithPackage :: Typed.TypedTerm Syntax.OrdinaryCompilationUnit -> Typed.TypedTerm (Maybe Syntax.PackageDeclaration) -> Typed.TypedTerm Syntax.OrdinaryCompilationUnit
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
ordinaryCompilationUnitWithTypes :: Typed.TypedTerm Syntax.OrdinaryCompilationUnit -> Typed.TypedTerm [Syntax.TopLevelClassOrInterfaceDeclarationWithComments] -> Typed.TypedTerm Syntax.OrdinaryCompilationUnit
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
packageDeclaration :: Typed.TypedTerm [Syntax.PackageModifier] -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.PackageDeclaration
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
packageDeclarationIdentifiers :: Typed.TypedTerm Syntax.PackageDeclaration -> Typed.TypedTerm [Syntax.Identifier]
packageDeclarationIdentifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
        Core.projectionFieldName = (Core.Name "identifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationModifiers :: Typed.TypedTerm Syntax.PackageDeclaration -> Typed.TypedTerm [Syntax.PackageModifier]
packageDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationWithIdentifiers :: Typed.TypedTerm Syntax.PackageDeclaration -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.PackageDeclaration
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
packageDeclarationWithModifiers :: Typed.TypedTerm Syntax.PackageDeclaration -> Typed.TypedTerm [Syntax.PackageModifier] -> Typed.TypedTerm Syntax.PackageDeclaration
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
packageModifier :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.PackageModifier
packageModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PackageName wrapper
packageName :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.PackageName
packageName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PackageOrTypeName wrapper
packageOrTypeName :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.PackageOrTypeName
packageOrTypeName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageOrTypeName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the record variant of hydra.java.syntax.Pattern
patternRecord :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm Syntax.Pattern
patternRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.Pattern
patternType :: Typed.TypedTerm Syntax.TypePattern -> Typed.TypedTerm Syntax.Pattern
patternType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.PostDecrementExpression wrapper
postDecrementExpression :: Typed.TypedTerm Syntax.PostfixExpression -> Typed.TypedTerm Syntax.PostDecrementExpression
postDecrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PostDecrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PostIncrementExpression wrapper
postIncrementExpression :: Typed.TypedTerm Syntax.PostfixExpression -> Typed.TypedTerm Syntax.PostIncrementExpression
postIncrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PostIncrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the name variant of hydra.java.syntax.PostfixExpression
postfixExpressionName :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.PostfixExpression
postfixExpressionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postDecrement variant of hydra.java.syntax.PostfixExpression
postfixExpressionPostDecrement :: Typed.TypedTerm Syntax.PostDecrementExpression -> Typed.TypedTerm Syntax.PostfixExpression
postfixExpressionPostDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postIncrement variant of hydra.java.syntax.PostfixExpression
postfixExpressionPostIncrement :: Typed.TypedTerm Syntax.PostIncrementExpression -> Typed.TypedTerm Syntax.PostfixExpression
postfixExpressionPostIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.PostfixExpression
postfixExpressionPrimary :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.PostfixExpression
postfixExpressionPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.PreDecrementExpression wrapper
preDecrementExpression :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.PreDecrementExpression
preDecrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PreDecrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PreIncrementExpression wrapper
preIncrementExpression :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.PreIncrementExpression
preIncrementExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PreIncrementExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the arrayCreation variant of hydra.java.syntax.Primary
primaryArrayCreation :: Typed.TypedTerm Syntax.ArrayCreationExpression -> Typed.TypedTerm Syntax.Primary
primaryArrayCreation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayCreation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the noNewArray variant of hydra.java.syntax.Primary
primaryNoNewArray :: Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression -> Typed.TypedTerm Syntax.Primary
primaryNoNewArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noNewArray"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the arrayAccess variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionArrayAccess :: Typed.TypedTerm Syntax.ArrayAccess -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionArrayAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classInstance variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassInstance :: Typed.TypedTerm Syntax.ClassInstanceCreationExpression -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassInstance x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstance"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classLiteral variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassLiteral :: Typed.TypedTerm Syntax.ClassLiteral -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classLiteral"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dotThis variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionDotThis :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionDotThis x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dotThis"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionFieldAccess :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionFieldAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the methodInvocation variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodInvocation :: Typed.TypedTerm Syntax.MethodInvocation -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodInvocation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the methodReference variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodReference :: Typed.TypedTerm Syntax.MethodReference -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodReference x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodReference"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionParens :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionParens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the this variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionThis :: Typed.TypedTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionThis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.PrimitiveType
primitiveTypeBoolean :: Typed.TypedTerm Syntax.PrimitiveType
primitiveTypeBoolean =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the numeric variant of hydra.java.syntax.PrimitiveType
primitiveTypeNumeric :: Typed.TypedTerm Syntax.NumericType -> Typed.TypedTerm Syntax.PrimitiveType
primitiveTypeNumeric x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotations :: Typed.TypedTerm Syntax.PrimitiveType -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations
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
primitiveTypeWithAnnotationsAnnotations :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm [Syntax.Annotation]
primitiveTypeWithAnnotationsAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsType :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.PrimitiveType
primitiveTypeWithAnnotationsType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithAnnotations :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations
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
primitiveTypeWithAnnotationsWithType :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.PrimitiveType -> Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations
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
receiverParameter :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.ReceiverParameter
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
receiverParameterAnnotations :: Typed.TypedTerm Syntax.ReceiverParameter -> Typed.TypedTerm [Syntax.Annotation]
receiverParameterAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ReceiverParameter
receiverParameterIdentifier :: Typed.TypedTerm Syntax.ReceiverParameter -> Typed.TypedTerm (Maybe Syntax.Identifier)
receiverParameterIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the unannType field of hydra.java.syntax.ReceiverParameter
receiverParameterUnannType :: Typed.TypedTerm Syntax.ReceiverParameter -> Typed.TypedTerm Syntax.UnannType
receiverParameterUnannType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionFieldName = (Core.Name "unannType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.ReceiverParameter
receiverParameterWithAnnotations :: Typed.TypedTerm Syntax.ReceiverParameter -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.ReceiverParameter
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
receiverParameterWithIdentifier :: Typed.TypedTerm Syntax.ReceiverParameter -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.ReceiverParameter
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
receiverParameterWithUnannType :: Typed.TypedTerm Syntax.ReceiverParameter -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.ReceiverParameter
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
recordBody :: Typed.TypedTerm [Syntax.RecordBodyDeclaration] -> Typed.TypedTerm Syntax.RecordBody
recordBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the classBody variant of hydra.java.syntax.RecordBodyDeclaration
recordBodyDeclarationClassBody :: Typed.TypedTerm Syntax.ClassBodyDeclaration -> Typed.TypedTerm Syntax.RecordBodyDeclaration
recordBodyDeclarationClassBody x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classBody"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the compactConstructor variant of hydra.java.syntax.RecordBodyDeclaration
recordBodyDeclarationCompactConstructor :: Typed.TypedTerm Syntax.CompactConstructorDeclaration -> Typed.TypedTerm Syntax.RecordBodyDeclaration
recordBodyDeclarationCompactConstructor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compactConstructor"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.RecordComponentModifier wrapper
recordComponentModifier :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.RecordComponentModifier
recordComponentModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordComponentModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.RecordComponent
recordComponentSimple :: Typed.TypedTerm Syntax.RecordComponent_Simple -> Typed.TypedTerm Syntax.RecordComponent
recordComponentSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.RecordComponent
recordComponentVariableArity :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm Syntax.RecordComponent
recordComponentVariableArity x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.RecordComponent_Simple
recordComponent_Simple :: Typed.TypedTerm [Syntax.RecordComponentModifier] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.RecordComponent_Simple
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
recordComponent_SimpleIdentifier :: Typed.TypedTerm Syntax.RecordComponent_Simple -> Typed.TypedTerm Syntax.Identifier
recordComponent_SimpleIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleModifiers :: Typed.TypedTerm Syntax.RecordComponent_Simple -> Typed.TypedTerm [Syntax.RecordComponentModifier]
recordComponent_SimpleModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleType :: Typed.TypedTerm Syntax.RecordComponent_Simple -> Typed.TypedTerm Syntax.UnannType
recordComponent_SimpleType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleWithIdentifier :: Typed.TypedTerm Syntax.RecordComponent_Simple -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.RecordComponent_Simple
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
recordComponent_SimpleWithModifiers :: Typed.TypedTerm Syntax.RecordComponent_Simple -> Typed.TypedTerm [Syntax.RecordComponentModifier] -> Typed.TypedTerm Syntax.RecordComponent_Simple
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
recordComponent_SimpleWithType :: Typed.TypedTerm Syntax.RecordComponent_Simple -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.RecordComponent_Simple
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
recordDeclaration :: Typed.TypedTerm [Syntax.ClassModifier] -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.RecordHeader -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm Syntax.RecordDeclaration
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
recordDeclarationBody :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm Syntax.RecordBody
recordDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.RecordDeclaration
recordDeclarationHeader :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm Syntax.RecordHeader
recordDeclarationHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.RecordDeclaration
recordDeclarationIdentifier :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier
recordDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.RecordDeclaration
recordDeclarationImplements :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm [Syntax.InterfaceType]
recordDeclarationImplements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "implements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.RecordDeclaration
recordDeclarationModifiers :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm [Syntax.ClassModifier]
recordDeclarationModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.RecordDeclaration
recordDeclarationParameters :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm [Syntax.TypeParameter]
recordDeclarationParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithBody :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm Syntax.RecordDeclaration
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
recordDeclarationWithHeader :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm Syntax.RecordHeader -> Typed.TypedTerm Syntax.RecordDeclaration
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
recordDeclarationWithIdentifier :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.RecordDeclaration
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
recordDeclarationWithImplements :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm [Syntax.InterfaceType] -> Typed.TypedTerm Syntax.RecordDeclaration
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
recordDeclarationWithModifiers :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm [Syntax.ClassModifier] -> Typed.TypedTerm Syntax.RecordDeclaration
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
recordDeclarationWithParameters :: Typed.TypedTerm Syntax.RecordDeclaration -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.RecordDeclaration
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
recordHeader :: Typed.TypedTerm [Syntax.RecordComponent] -> Typed.TypedTerm Syntax.RecordHeader
recordHeader x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordHeader"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.RecordPattern
recordPattern :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.RecordPattern
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
recordPatternPatterns :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm [Syntax.Pattern]
recordPatternPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.RecordPattern
recordPatternType :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm Syntax.ReferenceType
recordPatternType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the patterns field of hydra.java.syntax.RecordPattern
recordPatternWithPatterns :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.RecordPattern
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
recordPatternWithType :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.RecordPattern
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
referenceTypeArray :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.ReferenceType
referenceTypeArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ReferenceType
referenceTypeClassOrInterface :: Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm Syntax.ReferenceType
referenceTypeClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ReferenceType
referenceTypeVariable :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm Syntax.ReferenceType
referenceTypeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the greaterThan variant of hydra.java.syntax.RelationalExpression
relationalExpressionGreaterThan :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThan -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpressionGreaterThan x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the greaterThanEqual variant of hydra.java.syntax.RelationalExpression
relationalExpressionGreaterThanEqual :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpressionGreaterThanEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanEqual"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the instanceofExpression variant of hydra.java.syntax.RelationalExpression
relationalExpressionInstanceofExpression :: Typed.TypedTerm Syntax.InstanceofExpression -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpressionInstanceofExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceofExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lessThan variant of hydra.java.syntax.RelationalExpression
relationalExpressionLessThan :: Typed.TypedTerm Syntax.RelationalExpression_LessThan -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpressionLessThan x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lessThanEqual variant of hydra.java.syntax.RelationalExpression
relationalExpressionLessThanEqual :: Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpressionLessThanEqual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanEqual"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.RelationalExpression
relationalExpressionSimple :: Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThan :: Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_GreaterThan
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
relationalExpression_GreaterThanEqual :: Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual
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
relationalExpression_GreaterThanEqualLhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpression_GreaterThanEqualLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualRhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual -> Typed.TypedTerm Syntax.ShiftExpression
relationalExpression_GreaterThanEqualRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithLhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual -> Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual
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
relationalExpression_GreaterThanEqualWithRhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_GreaterThanEqual
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
relationalExpression_GreaterThanLhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThan -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpression_GreaterThanLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanRhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThan -> Typed.TypedTerm Syntax.ShiftExpression
relationalExpression_GreaterThanRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithLhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThan -> Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.RelationalExpression_GreaterThan
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
relationalExpression_GreaterThanWithRhs :: Typed.TypedTerm Syntax.RelationalExpression_GreaterThan -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_GreaterThan
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
relationalExpression_LessThan :: Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_LessThan
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
relationalExpression_LessThanEqual :: Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual
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
relationalExpression_LessThanEqualLhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpression_LessThanEqualLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualRhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual -> Typed.TypedTerm Syntax.ShiftExpression
relationalExpression_LessThanEqualRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithLhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual -> Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual
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
relationalExpression_LessThanEqualWithRhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_LessThanEqual
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
relationalExpression_LessThanLhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThan -> Typed.TypedTerm Syntax.RelationalExpression
relationalExpression_LessThanLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanRhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThan -> Typed.TypedTerm Syntax.ShiftExpression
relationalExpression_LessThanRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithLhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThan -> Typed.TypedTerm Syntax.RelationalExpression -> Typed.TypedTerm Syntax.RelationalExpression_LessThan
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
relationalExpression_LessThanWithRhs :: Typed.TypedTerm Syntax.RelationalExpression_LessThan -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.RelationalExpression_LessThan
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
requiresModifierStatic :: Typed.TypedTerm Syntax.RequiresModifier
requiresModifierStatic =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transitive variant of hydra.java.syntax.RequiresModifier
requiresModifierTransitive :: Typed.TypedTerm Syntax.RequiresModifier
requiresModifierTransitive =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transitive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the local variant of hydra.java.syntax.Resource
resourceLocal :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm Syntax.Resource
resourceLocal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ResourceSpecification wrapper
resourceSpecification :: Typed.TypedTerm [Syntax.Resource] -> Typed.TypedTerm Syntax.ResourceSpecification
resourceSpecification x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ResourceSpecification"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the variable variant of hydra.java.syntax.Resource
resourceVariable :: Typed.TypedTerm Syntax.VariableAccess -> Typed.TypedTerm Syntax.Resource
resourceVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.Resource_Local
resource_Local :: Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.LocalVariableType -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Resource_Local
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
resource_LocalExpression :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm Syntax.Expression
resource_LocalExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.Resource_Local
resource_LocalIdentifier :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm Syntax.Identifier
resource_LocalIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.Resource_Local
resource_LocalModifiers :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm [Syntax.VariableModifier]
resource_LocalModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.Resource_Local
resource_LocalType :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm Syntax.LocalVariableType
resource_LocalType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.Resource_Local
resource_LocalWithExpression :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Resource_Local
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
resource_LocalWithIdentifier :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Resource_Local
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
resource_LocalWithModifiers :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.Resource_Local
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
resource_LocalWithType :: Typed.TypedTerm Syntax.Resource_Local -> Typed.TypedTerm Syntax.LocalVariableType -> Typed.TypedTerm Syntax.Resource_Local
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
resultType :: Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.Result
resultType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the void variant of hydra.java.syntax.Result
resultVoid :: Typed.TypedTerm Syntax.Result
resultVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.ReturnStatement wrapper
returnStatement :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ReturnStatement
returnStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ReturnStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the shiftLeft variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftLeft :: Typed.TypedTerm Syntax.ShiftExpression_Binary -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpressionShiftLeft x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the shiftRight variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftRight :: Typed.TypedTerm Syntax.ShiftExpression_Binary -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpressionShiftRight x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the shiftRightZeroFill variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftRightZeroFill :: Typed.TypedTerm Syntax.ShiftExpression_Binary -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpressionShiftRightZeroFill x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.ShiftExpression
shiftExpressionUnary :: Typed.TypedTerm Syntax.AdditiveExpression -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ShiftExpression_Binary
shiftExpression_Binary :: Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.AdditiveExpression -> Typed.TypedTerm Syntax.ShiftExpression_Binary
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
shiftExpression_BinaryLhs :: Typed.TypedTerm Syntax.ShiftExpression_Binary -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpression_BinaryLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryRhs :: Typed.TypedTerm Syntax.ShiftExpression_Binary -> Typed.TypedTerm Syntax.AdditiveExpression
shiftExpression_BinaryRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryWithLhs :: Typed.TypedTerm Syntax.ShiftExpression_Binary -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.ShiftExpression_Binary
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
shiftExpression_BinaryWithRhs :: Typed.TypedTerm Syntax.ShiftExpression_Binary -> Typed.TypedTerm Syntax.AdditiveExpression -> Typed.TypedTerm Syntax.ShiftExpression_Binary
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
simpleTypeName :: Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.SimpleTypeName
simpleTypeName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.SimpleTypeName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.SingleElementAnnotation
singleElementAnnotation :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm (Maybe Syntax.ElementValue) -> Typed.TypedTerm Syntax.SingleElementAnnotation
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
singleElementAnnotationName :: Typed.TypedTerm Syntax.SingleElementAnnotation -> Typed.TypedTerm Syntax.TypeName
singleElementAnnotationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationValue :: Typed.TypedTerm Syntax.SingleElementAnnotation -> Typed.TypedTerm (Maybe Syntax.ElementValue)
singleElementAnnotationValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationWithName :: Typed.TypedTerm Syntax.SingleElementAnnotation -> Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.SingleElementAnnotation
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
singleElementAnnotationWithValue :: Typed.TypedTerm Syntax.SingleElementAnnotation -> Typed.TypedTerm (Maybe Syntax.ElementValue) -> Typed.TypedTerm Syntax.SingleElementAnnotation
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
singleStaticImportDeclaration :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.SingleStaticImportDeclaration
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
singleStaticImportDeclarationIdentifier :: Typed.TypedTerm Syntax.SingleStaticImportDeclaration -> Typed.TypedTerm Syntax.Identifier
singleStaticImportDeclarationIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationTypeName :: Typed.TypedTerm Syntax.SingleStaticImportDeclaration -> Typed.TypedTerm Syntax.TypeName
singleStaticImportDeclarationTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithIdentifier :: Typed.TypedTerm Syntax.SingleStaticImportDeclaration -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.SingleStaticImportDeclaration
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
singleStaticImportDeclarationWithTypeName :: Typed.TypedTerm Syntax.SingleStaticImportDeclaration -> Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.SingleStaticImportDeclaration
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
singleTypeImportDeclaration :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.SingleTypeImportDeclaration
singleTypeImportDeclaration x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.SingleTypeImportDeclaration"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.java.syntax.StatementExpression
statementExpressionAssignment :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.StatementExpression
statementExpressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classInstanceCreation variant of hydra.java.syntax.StatementExpression
statementExpressionClassInstanceCreation :: Typed.TypedTerm Syntax.ClassInstanceCreationExpression -> Typed.TypedTerm Syntax.StatementExpression
statementExpressionClassInstanceCreation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstanceCreation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the methodInvocation variant of hydra.java.syntax.StatementExpression
statementExpressionMethodInvocation :: Typed.TypedTerm Syntax.MethodInvocation -> Typed.TypedTerm Syntax.StatementExpression
statementExpressionMethodInvocation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postDecrement variant of hydra.java.syntax.StatementExpression
statementExpressionPostDecrement :: Typed.TypedTerm Syntax.PostDecrementExpression -> Typed.TypedTerm Syntax.StatementExpression
statementExpressionPostDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postIncrement variant of hydra.java.syntax.StatementExpression
statementExpressionPostIncrement :: Typed.TypedTerm Syntax.PostIncrementExpression -> Typed.TypedTerm Syntax.StatementExpression
statementExpressionPostIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preDecrement variant of hydra.java.syntax.StatementExpression
statementExpressionPreDecrement :: Typed.TypedTerm Syntax.PreDecrementExpression -> Typed.TypedTerm Syntax.StatementExpression
statementExpressionPreDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preIncrement variant of hydra.java.syntax.StatementExpression
statementExpressionPreIncrement :: Typed.TypedTerm Syntax.PreIncrementExpression -> Typed.TypedTerm Syntax.StatementExpression
statementExpressionPreIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.java.syntax.Statement
statementFor :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Syntax.Statement
statementFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ifThen variant of hydra.java.syntax.Statement
statementIfThen :: Typed.TypedTerm Syntax.IfThenStatement -> Typed.TypedTerm Syntax.Statement
statementIfThen x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThen"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ifThenElse variant of hydra.java.syntax.Statement
statementIfThenElse :: Typed.TypedTerm Syntax.IfThenElseStatement -> Typed.TypedTerm Syntax.Statement
statementIfThenElse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the labeled variant of hydra.java.syntax.Statement
statementLabeled :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Statement
statementLabeled x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfFor :: Typed.TypedTerm Syntax.ForStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
statementNoShortIfFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ifThenElse variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfIfThenElse :: Typed.TypedTerm Syntax.IfThenElseStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
statementNoShortIfIfThenElse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the labeled variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfLabeled :: Typed.TypedTerm Syntax.LabeledStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
statementNoShortIfLabeled x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfWhile :: Typed.TypedTerm Syntax.WhileStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
statementNoShortIfWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withoutTrailing variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfWithoutTrailing :: Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement -> Typed.TypedTerm Syntax.StatementNoShortIf
statementNoShortIfWithoutTrailing x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.java.syntax.Statement
statementWhile :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Statement
statementWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withoutTrailing variant of hydra.java.syntax.Statement
statementWithoutTrailing :: Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement -> Typed.TypedTerm Syntax.Statement
statementWithoutTrailing x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assert variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementAssert :: Typed.TypedTerm Syntax.AssertStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementAssert x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assert"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the block variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the break variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBreak :: Typed.TypedTerm Syntax.BreakStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBreak x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the continue variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementContinue :: Typed.TypedTerm Syntax.ContinueStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementContinue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the do variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementDo :: Typed.TypedTerm Syntax.DoStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementDo x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the empty variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementEmpty :: Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementEmpty =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the expression variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementExpression :: Typed.TypedTerm Syntax.ExpressionStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the return variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementReturn :: Typed.TypedTerm Syntax.ReturnStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementReturn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the switch variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSwitch :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSwitch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the synchronized variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSynchronized :: Typed.TypedTerm Syntax.SynchronizedStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSynchronized x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the throw variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementThrow :: Typed.TypedTerm Syntax.ThrowStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementThrow x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the try variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementTry :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementTry x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the yield variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementYield :: Typed.TypedTerm Syntax.YieldStatement -> Typed.TypedTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.StaticImportOnDemandDeclaration wrapper
staticImportOnDemandDeclaration :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.StaticImportOnDemandDeclaration
staticImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StaticImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.StaticInitializer wrapper
staticInitializer :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.StaticInitializer
staticInitializer x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StaticInitializer"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.StringLiteral wrapper
stringLiteral :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.StringLiteral
stringLiteral x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StringLiteral"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the legacy variant of hydra.java.syntax.SwitchBlock
switchBlockLegacy :: Typed.TypedTerm Syntax.SwitchBlock_Legacy -> Typed.TypedTerm Syntax.SwitchBlock
switchBlockLegacy x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "legacy"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rules variant of hydra.java.syntax.SwitchBlock
switchBlockRules :: Typed.TypedTerm [Syntax.SwitchRule] -> Typed.TypedTerm Syntax.SwitchBlock
switchBlockRules x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rules"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroup :: Typed.TypedTerm [Syntax.SwitchLabel] -> Typed.TypedTerm [Syntax.BlockStatement] -> Typed.TypedTerm Syntax.SwitchBlockStatementGroup
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
switchBlockStatementGroupLabels :: Typed.TypedTerm Syntax.SwitchBlockStatementGroup -> Typed.TypedTerm [Syntax.SwitchLabel]
switchBlockStatementGroupLabels x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionFieldName = (Core.Name "labels")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statements field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupStatements :: Typed.TypedTerm Syntax.SwitchBlockStatementGroup -> Typed.TypedTerm [Syntax.BlockStatement]
switchBlockStatementGroupStatements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the labels field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithLabels :: Typed.TypedTerm Syntax.SwitchBlockStatementGroup -> Typed.TypedTerm [Syntax.SwitchLabel] -> Typed.TypedTerm Syntax.SwitchBlockStatementGroup
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
switchBlockStatementGroupWithStatements :: Typed.TypedTerm Syntax.SwitchBlockStatementGroup -> Typed.TypedTerm [Syntax.BlockStatement] -> Typed.TypedTerm Syntax.SwitchBlockStatementGroup
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
switchBlock_Legacy :: Typed.TypedTerm [Syntax.SwitchBlockStatementGroup] -> Typed.TypedTerm [Syntax.SwitchLabel] -> Typed.TypedTerm Syntax.SwitchBlock_Legacy
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
switchBlock_LegacyGroups :: Typed.TypedTerm Syntax.SwitchBlock_Legacy -> Typed.TypedTerm [Syntax.SwitchBlockStatementGroup]
switchBlock_LegacyGroups x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
        Core.projectionFieldName = (Core.Name "groups")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the trailingLabels field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyTrailingLabels :: Typed.TypedTerm Syntax.SwitchBlock_Legacy -> Typed.TypedTerm [Syntax.SwitchLabel]
switchBlock_LegacyTrailingLabels x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
        Core.projectionFieldName = (Core.Name "trailingLabels")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the groups field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyWithGroups :: Typed.TypedTerm Syntax.SwitchBlock_Legacy -> Typed.TypedTerm [Syntax.SwitchBlockStatementGroup] -> Typed.TypedTerm Syntax.SwitchBlock_Legacy
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
switchBlock_LegacyWithTrailingLabels :: Typed.TypedTerm Syntax.SwitchBlock_Legacy -> Typed.TypedTerm [Syntax.SwitchLabel] -> Typed.TypedTerm Syntax.SwitchBlock_Legacy
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
switchExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SwitchBlock -> Typed.TypedTerm Syntax.SwitchExpression
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
switchExpressionBlock :: Typed.TypedTerm Syntax.SwitchExpression -> Typed.TypedTerm Syntax.SwitchBlock
switchExpressionBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.SwitchExpression
switchExpressionCond :: Typed.TypedTerm Syntax.SwitchExpression -> Typed.TypedTerm Syntax.Expression
switchExpressionCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SwitchExpression
switchExpressionWithBlock :: Typed.TypedTerm Syntax.SwitchExpression -> Typed.TypedTerm Syntax.SwitchBlock -> Typed.TypedTerm Syntax.SwitchExpression
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
switchExpressionWithCond :: Typed.TypedTerm Syntax.SwitchExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SwitchExpression
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
switchLabelCase :: Typed.TypedTerm [Syntax.CaseConstant] -> Typed.TypedTerm Syntax.SwitchLabel
switchLabelCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the casePattern variant of hydra.java.syntax.SwitchLabel
switchLabelCasePattern :: Typed.TypedTerm Syntax.CasePattern -> Typed.TypedTerm Syntax.SwitchLabel
switchLabelCasePattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "casePattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the default variant of hydra.java.syntax.SwitchLabel
switchLabelDefault :: Typed.TypedTerm Syntax.SwitchLabel
switchLabelDefault =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the null variant of hydra.java.syntax.SwitchLabel
switchLabelNull :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.SwitchLabel
switchLabelNull x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchRule
switchRule :: Typed.TypedTerm Syntax.SwitchLabel -> Typed.TypedTerm Syntax.SwitchRule_Body -> Typed.TypedTerm Syntax.SwitchRule
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
switchRuleBody :: Typed.TypedTerm Syntax.SwitchRule -> Typed.TypedTerm Syntax.SwitchRule_Body
switchRuleBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.java.syntax.SwitchRule
switchRuleLabel :: Typed.TypedTerm Syntax.SwitchRule -> Typed.TypedTerm Syntax.SwitchLabel
switchRuleLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.SwitchRule
switchRuleWithBody :: Typed.TypedTerm Syntax.SwitchRule -> Typed.TypedTerm Syntax.SwitchRule_Body -> Typed.TypedTerm Syntax.SwitchRule
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
switchRuleWithLabel :: Typed.TypedTerm Syntax.SwitchRule -> Typed.TypedTerm Syntax.SwitchLabel -> Typed.TypedTerm Syntax.SwitchRule
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
switchRule_BodyBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.SwitchRule_Body
switchRule_BodyBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SwitchRule_Body
switchRule_BodyExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the throw variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyThrow :: Typed.TypedTerm Syntax.ThrowStatement -> Typed.TypedTerm Syntax.SwitchRule_Body
switchRule_BodyThrow x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchStatement
switchStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SwitchBlock -> Typed.TypedTerm Syntax.SwitchStatement
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
switchStatementBlock :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.SwitchBlock
switchStatementBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.SwitchStatement
switchStatementCond :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.Expression
switchStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SwitchStatement
switchStatementWithBlock :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.SwitchBlock -> Typed.TypedTerm Syntax.SwitchStatement
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
switchStatementWithCond :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SwitchStatement
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
synchronizedStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.SynchronizedStatement
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
synchronizedStatementBlock :: Typed.TypedTerm Syntax.SynchronizedStatement -> Typed.TypedTerm Syntax.Block
synchronizedStatementBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementExpression :: Typed.TypedTerm Syntax.SynchronizedStatement -> Typed.TypedTerm Syntax.Expression
synchronizedStatementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementWithBlock :: Typed.TypedTerm Syntax.SynchronizedStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.SynchronizedStatement
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
synchronizedStatementWithExpression :: Typed.TypedTerm Syntax.SynchronizedStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SynchronizedStatement
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
textBlock :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.TextBlock
textBlock x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TextBlock"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.ThrowStatement wrapper
throwStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ThrowStatement
throwStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ThrowStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.Throws wrapper
throws :: Typed.TypedTerm [Syntax.ExceptionType] -> Typed.TypedTerm Syntax.Throws
throws x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Throws"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the class variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationClass :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationInterface :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationNone :: Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithComments :: Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclaration -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments
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
topLevelClassOrInterfaceDeclarationWithCommentsComments :: Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Typed.TypedTerm (Maybe String)
topLevelClassOrInterfaceDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsValue :: Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationWithCommentsValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsWithComments :: Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments
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
topLevelClassOrInterfaceDeclarationWithCommentsWithValue :: Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclaration -> Typed.TypedTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments
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
tryStatementSimple :: Typed.TypedTerm Syntax.TryStatement_Simple -> Typed.TypedTerm Syntax.TryStatement
tryStatementSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withFinally variant of hydra.java.syntax.TryStatement
tryStatementWithFinally :: Typed.TypedTerm Syntax.TryStatement_WithFinally -> Typed.TypedTerm Syntax.TryStatement
tryStatementWithFinally x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withFinally"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the withResources variant of hydra.java.syntax.TryStatement
tryStatementWithResources :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm Syntax.TryStatement
tryStatementWithResources x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withResources"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TryStatement_Simple
tryStatement_Simple :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.Catches -> Typed.TypedTerm Syntax.TryStatement_Simple
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
tryStatement_SimpleBlock :: Typed.TypedTerm Syntax.TryStatement_Simple -> Typed.TypedTerm Syntax.Block
tryStatement_SimpleBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleCatches :: Typed.TypedTerm Syntax.TryStatement_Simple -> Typed.TypedTerm Syntax.Catches
tryStatement_SimpleCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
        Core.projectionFieldName = (Core.Name "catches")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleWithBlock :: Typed.TypedTerm Syntax.TryStatement_Simple -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryStatement_Simple
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
tryStatement_SimpleWithCatches :: Typed.TypedTerm Syntax.TryStatement_Simple -> Typed.TypedTerm Syntax.Catches -> Typed.TypedTerm Syntax.TryStatement_Simple
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
tryStatement_WithFinally :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm (Maybe Syntax.Catches) -> Typed.TypedTerm Syntax.Finally -> Typed.TypedTerm Syntax.TryStatement_WithFinally
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
tryStatement_WithFinallyBlock :: Typed.TypedTerm Syntax.TryStatement_WithFinally -> Typed.TypedTerm Syntax.Block
tryStatement_WithFinallyBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyCatches :: Typed.TypedTerm Syntax.TryStatement_WithFinally -> Typed.TypedTerm (Maybe Syntax.Catches)
tryStatement_WithFinallyCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionFieldName = (Core.Name "catches")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyFinally :: Typed.TypedTerm Syntax.TryStatement_WithFinally -> Typed.TypedTerm Syntax.Finally
tryStatement_WithFinallyFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithBlock :: Typed.TypedTerm Syntax.TryStatement_WithFinally -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryStatement_WithFinally
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
tryStatement_WithFinallyWithCatches :: Typed.TypedTerm Syntax.TryStatement_WithFinally -> Typed.TypedTerm (Maybe Syntax.Catches) -> Typed.TypedTerm Syntax.TryStatement_WithFinally
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
tryStatement_WithFinallyWithFinally :: Typed.TypedTerm Syntax.TryStatement_WithFinally -> Typed.TypedTerm Syntax.Finally -> Typed.TypedTerm Syntax.TryStatement_WithFinally
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
tryWithResourcesStatement :: Typed.TypedTerm Syntax.ResourceSpecification -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm (Maybe Syntax.Catches) -> Typed.TypedTerm (Maybe Syntax.Finally) -> Typed.TypedTerm Syntax.TryWithResourcesStatement
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
tryWithResourcesStatementBlock :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm Syntax.Block
tryWithResourcesStatementBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementCatches :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm (Maybe Syntax.Catches)
tryWithResourcesStatementCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "catches")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementFinally :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm (Maybe Syntax.Finally)
tryWithResourcesStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the resourceSpecification field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementResourceSpecification :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm Syntax.ResourceSpecification
tryWithResourcesStatementResourceSpecification x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionFieldName = (Core.Name "resourceSpecification")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithBlock :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryWithResourcesStatement
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
tryWithResourcesStatementWithCatches :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm (Maybe Syntax.Catches) -> Typed.TypedTerm Syntax.TryWithResourcesStatement
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
tryWithResourcesStatementWithFinally :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm (Maybe Syntax.Finally) -> Typed.TypedTerm Syntax.TryWithResourcesStatement
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
tryWithResourcesStatementWithResourceSpecification :: Typed.TypedTerm Syntax.TryWithResourcesStatement -> Typed.TypedTerm Syntax.ResourceSpecification -> Typed.TypedTerm Syntax.TryWithResourcesStatement
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
typeArgumentReference :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.TypeArgument
typeArgumentReference x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.java.syntax.TypeArgument
typeArgumentWildcard :: Typed.TypedTerm Syntax.Wildcard -> Typed.TypedTerm Syntax.TypeArgument
typeArgumentWildcard x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the arguments variant of hydra.java.syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondArguments :: Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondArguments x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arguments"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the diamond variant of hydra.java.syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondDiamond :: Typed.TypedTerm Syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondDiamond =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "diamond"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.TypeBound
typeBoundClassOrInterface :: Typed.TypedTerm Syntax.TypeBound_ClassOrInterface -> Typed.TypedTerm Syntax.TypeBound
typeBoundClassOrInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.TypeBound
typeBoundVariable :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm Syntax.TypeBound
typeBoundVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterface :: Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm [Syntax.AdditionalBound] -> Typed.TypedTerm Syntax.TypeBound_ClassOrInterface
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
typeBound_ClassOrInterfaceAdditional :: Typed.TypedTerm Syntax.TypeBound_ClassOrInterface -> Typed.TypedTerm [Syntax.AdditionalBound]
typeBound_ClassOrInterfaceAdditional x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "additional")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceType :: Typed.TypedTerm Syntax.TypeBound_ClassOrInterface -> Typed.TypedTerm Syntax.ClassOrInterfaceType
typeBound_ClassOrInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the additional field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithAdditional :: Typed.TypedTerm Syntax.TypeBound_ClassOrInterface -> Typed.TypedTerm [Syntax.AdditionalBound] -> Typed.TypedTerm Syntax.TypeBound_ClassOrInterface
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
typeBound_ClassOrInterfaceWithType :: Typed.TypedTerm Syntax.TypeBound_ClassOrInterface -> Typed.TypedTerm Syntax.ClassOrInterfaceType -> Typed.TypedTerm Syntax.TypeBound_ClassOrInterface
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
typeIdentifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.TypeIdentifier
typeIdentifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeIdentifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.TypeImportOnDemandDeclaration wrapper
typeImportOnDemandDeclaration :: Typed.TypedTerm Syntax.PackageOrTypeName -> Typed.TypedTerm Syntax.TypeImportOnDemandDeclaration
typeImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.TypeName
typeName :: Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm (Maybe Syntax.PackageOrTypeName) -> Typed.TypedTerm Syntax.TypeName
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
typeNameArrayArray :: Typed.TypedTerm Syntax.TypeNameArray -> Typed.TypedTerm Syntax.TypeNameArray
typeNameArrayArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.TypeNameArray
typeNameArraySimple :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.TypeNameArray
typeNameArraySimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeName
typeNameIdentifier :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.TypeIdentifier
typeNameIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.TypeName
typeNameQualifier :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm (Maybe Syntax.PackageOrTypeName)
typeNameQualifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "qualifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.TypeName
typeNameWithIdentifier :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.TypeName
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
typeNameWithQualifier :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm (Maybe Syntax.PackageOrTypeName) -> Typed.TypedTerm Syntax.TypeName
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
typeParameter :: Typed.TypedTerm [Syntax.TypeParameterModifier] -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm (Maybe Syntax.TypeBound) -> Typed.TypedTerm Syntax.TypeParameter
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
typeParameterBound :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm (Maybe Syntax.TypeBound)
typeParameterBound x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "bound")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeParameter
typeParameterIdentifier :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm Syntax.TypeIdentifier
typeParameterIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.TypeParameterModifier wrapper
typeParameterModifier :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.TypeParameterModifier
typeParameterModifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeParameterModifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.TypeParameter
typeParameterModifiers :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm [Syntax.TypeParameterModifier]
typeParameterModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bound field of hydra.java.syntax.TypeParameter
typeParameterWithBound :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm (Maybe Syntax.TypeBound) -> Typed.TypedTerm Syntax.TypeParameter
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
typeParameterWithIdentifier :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.TypeParameter
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
typeParameterWithModifiers :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm [Syntax.TypeParameterModifier] -> Typed.TypedTerm Syntax.TypeParameter
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
typePattern :: Typed.TypedTerm Syntax.LocalVariableDeclaration -> Typed.TypedTerm Syntax.TypePattern
typePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the primitive variant of hydra.java.syntax.Type
typePrimitive :: Typed.TypedTerm Syntax.PrimitiveTypeWithAnnotations -> Typed.TypedTerm Syntax.Type
typePrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the reference variant of hydra.java.syntax.Type
typeReference :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.Type
typeReference x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TypeVariable
typeVariable :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.TypeVariable
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
typeVariableAnnotations :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm [Syntax.Annotation]
typeVariableAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeVariable
typeVariableIdentifier :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm Syntax.TypeIdentifier
typeVariableIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.TypeVariable
typeVariableWithAnnotations :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.TypeVariable
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
typeVariableWithIdentifier :: Typed.TypedTerm Syntax.TypeVariable -> Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.TypeVariable
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
unAdditionalBound :: Typed.TypedTerm Syntax.AdditionalBound -> Typed.TypedTerm Syntax.InterfaceType
unAdditionalBound x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AdditionalBound")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AmbiguousName
unAmbiguousName :: Typed.TypedTerm Syntax.AmbiguousName -> Typed.TypedTerm [Syntax.Identifier]
unAmbiguousName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AmbiguousName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AndExpression
unAndExpression :: Typed.TypedTerm Syntax.AndExpression -> Typed.TypedTerm [Syntax.EqualityExpression]
unAndExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AndExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AnnotationInterfaceBody
unAnnotationInterfaceBody :: Typed.TypedTerm Syntax.AnnotationInterfaceBody -> Typed.TypedTerm [Syntax.AnnotationInterfaceMemberDeclaration]
unAnnotationInterfaceBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AnnotationInterfaceBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ArrayInitializer
unArrayInitializer :: Typed.TypedTerm Syntax.ArrayInitializer -> Typed.TypedTerm [[Syntax.VariableInitializer]]
unArrayInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ArrayInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Block
unBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm [Syntax.BlockStatement]
unBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Block")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.BreakStatement
unBreakStatement :: Typed.TypedTerm Syntax.BreakStatement -> Typed.TypedTerm (Maybe Syntax.Identifier)
unBreakStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.BreakStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.CaseConstant
unCaseConstant :: Typed.TypedTerm Syntax.CaseConstant -> Typed.TypedTerm Syntax.ConditionalExpression
unCaseConstant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.CaseConstant")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Catches
unCatches :: Typed.TypedTerm Syntax.Catches -> Typed.TypedTerm [Syntax.CatchClause]
unCatches x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Catches")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ClassBody
unClassBody :: Typed.TypedTerm Syntax.ClassBody -> Typed.TypedTerm [Syntax.ClassBodyDeclarationWithComments]
unClassBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ClassBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConditionalAndExpression
unConditionalAndExpression :: Typed.TypedTerm Syntax.ConditionalAndExpression -> Typed.TypedTerm [Syntax.InclusiveOrExpression]
unConditionalAndExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConditionalAndExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConditionalOrExpression
unConditionalOrExpression :: Typed.TypedTerm Syntax.ConditionalOrExpression -> Typed.TypedTerm [Syntax.ConditionalAndExpression]
unConditionalOrExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConditionalOrExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConstantExpression
unConstantExpression :: Typed.TypedTerm Syntax.ConstantExpression -> Typed.TypedTerm Syntax.Expression
unConstantExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConstantExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ContinueStatement
unContinueStatement :: Typed.TypedTerm Syntax.ContinueStatement -> Typed.TypedTerm (Maybe Syntax.Identifier)
unContinueStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ContinueStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.DefaultValue
unDefaultValue :: Typed.TypedTerm Syntax.DefaultValue -> Typed.TypedTerm Syntax.ElementValue
unDefaultValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.DefaultValue")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Dims
unDims :: Typed.TypedTerm Syntax.Dims -> Typed.TypedTerm [[Syntax.Annotation]]
unDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Dims")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ElementValueArrayInitializer
unElementValueArrayInitializer :: Typed.TypedTerm Syntax.ElementValueArrayInitializer -> Typed.TypedTerm [Syntax.ElementValue]
unElementValueArrayInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ElementValueArrayInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.EnumBody
unEnumBody :: Typed.TypedTerm Syntax.EnumBody -> Typed.TypedTerm [Syntax.EnumBody_Element]
unEnumBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.EnumBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.EnumConstantModifier
unEnumConstantModifier :: Typed.TypedTerm Syntax.EnumConstantModifier -> Typed.TypedTerm Syntax.Annotation
unEnumConstantModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.EnumConstantModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ExclusiveOrExpression
unExclusiveOrExpression :: Typed.TypedTerm Syntax.ExclusiveOrExpression -> Typed.TypedTerm [Syntax.AndExpression]
unExclusiveOrExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ExclusiveOrExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ExpressionStatement
unExpressionStatement :: Typed.TypedTerm Syntax.ExpressionStatement -> Typed.TypedTerm Syntax.StatementExpression
unExpressionStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ExpressionStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Finally
unFinally :: Typed.TypedTerm Syntax.Finally -> Typed.TypedTerm Syntax.Block
unFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Finally")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.FloatingPointLiteral
unFloatingPointLiteral :: Typed.TypedTerm Syntax.FloatingPointLiteral -> Typed.TypedTerm Double
unFloatingPointLiteral x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.FloatingPointLiteral")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ForUpdate
unForUpdate :: Typed.TypedTerm Syntax.ForUpdate -> Typed.TypedTerm [Syntax.StatementExpression]
unForUpdate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ForUpdate")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Guard
unGuard :: Typed.TypedTerm Syntax.Guard -> Typed.TypedTerm Syntax.Expression
unGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Guard")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Identifier
unIdentifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm String
unIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Identifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InclusiveOrExpression
unInclusiveOrExpression :: Typed.TypedTerm Syntax.InclusiveOrExpression -> Typed.TypedTerm [Syntax.ExclusiveOrExpression]
unInclusiveOrExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InclusiveOrExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InstanceInitializer
unInstanceInitializer :: Typed.TypedTerm Syntax.InstanceInitializer -> Typed.TypedTerm Syntax.Block
unInstanceInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InstanceInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.IntegerLiteral
unIntegerLiteral :: Typed.TypedTerm Syntax.IntegerLiteral -> Typed.TypedTerm Integer
unIntegerLiteral x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.IntegerLiteral")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InterfaceBody
unInterfaceBody :: Typed.TypedTerm Syntax.InterfaceBody -> Typed.TypedTerm [Syntax.InterfaceMemberDeclarationWithComments]
unInterfaceBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InterfaceBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InterfaceType
unInterfaceType :: Typed.TypedTerm Syntax.InterfaceType -> Typed.TypedTerm Syntax.ClassType
unInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InterfaceType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.LocalVariableDeclarationStatement
unLocalVariableDeclarationStatement :: Typed.TypedTerm Syntax.LocalVariableDeclarationStatement -> Typed.TypedTerm Syntax.LocalVariableDeclaration
unLocalVariableDeclarationStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.LocalVariableDeclarationStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MarkerAnnotation
unMarkerAnnotation :: Typed.TypedTerm Syntax.MarkerAnnotation -> Typed.TypedTerm Syntax.TypeName
unMarkerAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MarkerAnnotation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MethodName
unMethodName :: Typed.TypedTerm Syntax.MethodName -> Typed.TypedTerm Syntax.Identifier
unMethodName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MethodName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MethodReference_Array
unMethodReference_Array :: Typed.TypedTerm Syntax.MethodReference_Array -> Typed.TypedTerm Syntax.ArrayType
unMethodReference_Array x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MethodReference_Array")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageModifier
unPackageModifier :: Typed.TypedTerm Syntax.PackageModifier -> Typed.TypedTerm Syntax.Annotation
unPackageModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageName
unPackageName :: Typed.TypedTerm Syntax.PackageName -> Typed.TypedTerm [Syntax.Identifier]
unPackageName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageOrTypeName
unPackageOrTypeName :: Typed.TypedTerm Syntax.PackageOrTypeName -> Typed.TypedTerm [Syntax.Identifier]
unPackageOrTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageOrTypeName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PostDecrementExpression
unPostDecrementExpression :: Typed.TypedTerm Syntax.PostDecrementExpression -> Typed.TypedTerm Syntax.PostfixExpression
unPostDecrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PostDecrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PostIncrementExpression
unPostIncrementExpression :: Typed.TypedTerm Syntax.PostIncrementExpression -> Typed.TypedTerm Syntax.PostfixExpression
unPostIncrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PostIncrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PreDecrementExpression
unPreDecrementExpression :: Typed.TypedTerm Syntax.PreDecrementExpression -> Typed.TypedTerm Syntax.UnaryExpression
unPreDecrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PreDecrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PreIncrementExpression
unPreIncrementExpression :: Typed.TypedTerm Syntax.PreIncrementExpression -> Typed.TypedTerm Syntax.UnaryExpression
unPreIncrementExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PreIncrementExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordBody
unRecordBody :: Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm [Syntax.RecordBodyDeclaration]
unRecordBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordComponentModifier
unRecordComponentModifier :: Typed.TypedTerm Syntax.RecordComponentModifier -> Typed.TypedTerm Syntax.Annotation
unRecordComponentModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordComponentModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordHeader
unRecordHeader :: Typed.TypedTerm Syntax.RecordHeader -> Typed.TypedTerm [Syntax.RecordComponent]
unRecordHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordHeader")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ResourceSpecification
unResourceSpecification :: Typed.TypedTerm Syntax.ResourceSpecification -> Typed.TypedTerm [Syntax.Resource]
unResourceSpecification x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ResourceSpecification")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ReturnStatement
unReturnStatement :: Typed.TypedTerm Syntax.ReturnStatement -> Typed.TypedTerm (Maybe Syntax.Expression)
unReturnStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ReturnStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.SimpleTypeName
unSimpleTypeName :: Typed.TypedTerm Syntax.SimpleTypeName -> Typed.TypedTerm Syntax.TypeIdentifier
unSimpleTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.SimpleTypeName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.SingleTypeImportDeclaration
unSingleTypeImportDeclaration :: Typed.TypedTerm Syntax.SingleTypeImportDeclaration -> Typed.TypedTerm Syntax.TypeName
unSingleTypeImportDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.SingleTypeImportDeclaration")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StaticImportOnDemandDeclaration
unStaticImportOnDemandDeclaration :: Typed.TypedTerm Syntax.StaticImportOnDemandDeclaration -> Typed.TypedTerm Syntax.TypeName
unStaticImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StaticImportOnDemandDeclaration")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StaticInitializer
unStaticInitializer :: Typed.TypedTerm Syntax.StaticInitializer -> Typed.TypedTerm Syntax.Block
unStaticInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StaticInitializer")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StringLiteral
unStringLiteral :: Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm String
unStringLiteral x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StringLiteral")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TextBlock
unTextBlock :: Typed.TypedTerm Syntax.TextBlock -> Typed.TypedTerm String
unTextBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TextBlock")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ThrowStatement
unThrowStatement :: Typed.TypedTerm Syntax.ThrowStatement -> Typed.TypedTerm Syntax.Expression
unThrowStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ThrowStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Throws
unThrows :: Typed.TypedTerm Syntax.Throws -> Typed.TypedTerm [Syntax.ExceptionType]
unThrows x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Throws")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeIdentifier
unTypeIdentifier :: Typed.TypedTerm Syntax.TypeIdentifier -> Typed.TypedTerm Syntax.Identifier
unTypeIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeIdentifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeImportOnDemandDeclaration
unTypeImportOnDemandDeclaration :: Typed.TypedTerm Syntax.TypeImportOnDemandDeclaration -> Typed.TypedTerm Syntax.PackageOrTypeName
unTypeImportOnDemandDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeImportOnDemandDeclaration")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeParameterModifier
unTypeParameterModifier :: Typed.TypedTerm Syntax.TypeParameterModifier -> Typed.TypedTerm Syntax.Annotation
unTypeParameterModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeParameterModifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypePattern
unTypePattern :: Typed.TypedTerm Syntax.TypePattern -> Typed.TypedTerm Syntax.LocalVariableDeclaration
unTypePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.UnannClassType
unUnannClassType :: Typed.TypedTerm Syntax.UnannClassType -> Typed.TypedTerm Syntax.ClassType
unUnannClassType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.UnannClassType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.UnannType
unUnannType :: Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.Type
unUnannType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.UnannType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.YieldStatement
unYieldStatement :: Typed.TypedTerm Syntax.YieldStatement -> Typed.TypedTerm Syntax.Expression
unYieldStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.YieldStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.UnannClassType wrapper
unannClassType :: Typed.TypedTerm Syntax.ClassType -> Typed.TypedTerm Syntax.UnannClassType
unannClassType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.UnannClassType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.java.syntax.UnannType wrapper
unannType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.UnannType
unannType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.UnannType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the minus variant of hydra.java.syntax.UnaryExpression
unaryExpressionMinus :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionMinus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the cast variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusCast :: Typed.TypedTerm Syntax.CastExpression -> Typed.TypedTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusCast x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the not variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusNot :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusNot x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the postfix variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusPostfix :: Typed.TypedTerm Syntax.PostfixExpression -> Typed.TypedTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusPostfix x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postfix"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the switchExpression variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusSwitchExpression :: Typed.TypedTerm Syntax.SwitchExpression -> Typed.TypedTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusSwitchExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switchExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tilde variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusTilde :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusTilde x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tilde"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the other variant of hydra.java.syntax.UnaryExpression
unaryExpressionOther :: Typed.TypedTerm Syntax.UnaryExpressionNotPlusMinus -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the plus variant of hydra.java.syntax.UnaryExpression
unaryExpressionPlus :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionPlus x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preDecrement variant of hydra.java.syntax.UnaryExpression
unaryExpressionPreDecrement :: Typed.TypedTerm Syntax.PreDecrementExpression -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionPreDecrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the preIncrement variant of hydra.java.syntax.UnaryExpression
unaryExpressionPreIncrement :: Typed.TypedTerm Syntax.PreIncrementExpression -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionPreIncrement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression :: Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm (Maybe Syntax.ClassBody) -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression
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
unqualifiedClassInstanceCreationExpressionArguments :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm [Syntax.Expression]
unqualifiedClassInstanceCreationExpressionArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionBody :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm (Maybe Syntax.ClassBody)
unqualifiedClassInstanceCreationExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the classOrInterface field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionClassOrInterface :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate
unqualifiedClassInstanceCreationExpressionClassOrInterface x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "classOrInterface")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionTypeArguments :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm [Syntax.TypeArgument]
unqualifiedClassInstanceCreationExpressionTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithArguments :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression
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
unqualifiedClassInstanceCreationExpressionWithBody :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm (Maybe Syntax.ClassBody) -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression
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
unqualifiedClassInstanceCreationExpressionWithClassOrInterface :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression
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
unqualifiedClassInstanceCreationExpressionWithTypeArguments :: Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Typed.TypedTerm [Syntax.TypeArgument] -> Typed.TypedTerm Syntax.UnqualifiedClassInstanceCreationExpression
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
variableAccessExpressionName :: Typed.TypedTerm Syntax.ExpressionName -> Typed.TypedTerm Syntax.VariableAccess
variableAccessExpressionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.VariableAccess
variableAccessFieldAccess :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.VariableAccess
variableAccessFieldAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.java.syntax.VariableArityParameter
variableArityParameter :: Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.VariableArityParameter
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
variableArityParameterAnnotations :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm [Syntax.Annotation]
variableArityParameterAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableArityParameter
variableArityParameterIdentifier :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm Syntax.Identifier
variableArityParameterIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.VariableArityParameter
variableArityParameterModifiers :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm [Syntax.VariableModifier]
variableArityParameterModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.VariableArityParameter
variableArityParameterType :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm Syntax.UnannType
variableArityParameterType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithAnnotations :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.VariableArityParameter
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
variableArityParameterWithIdentifier :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.VariableArityParameter
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
variableArityParameterWithModifiers :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm [Syntax.VariableModifier] -> Typed.TypedTerm Syntax.VariableArityParameter
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
variableArityParameterWithType :: Typed.TypedTerm Syntax.VariableArityParameter -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.VariableArityParameter
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
variableArityRecordComponent :: Typed.TypedTerm [Syntax.RecordComponentModifier] -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.VariableArityRecordComponent
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
variableArityRecordComponentAnnotations :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm [Syntax.Annotation]
variableArityRecordComponentAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentIdentifier :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm Syntax.Identifier
variableArityRecordComponentIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentModifiers :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm [Syntax.RecordComponentModifier]
variableArityRecordComponentModifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "modifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentType :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm Syntax.UnannType
variableArityRecordComponentType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithAnnotations :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.VariableArityRecordComponent
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
variableArityRecordComponentWithIdentifier :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.VariableArityRecordComponent
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
variableArityRecordComponentWithModifiers :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm [Syntax.RecordComponentModifier] -> Typed.TypedTerm Syntax.VariableArityRecordComponent
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
variableArityRecordComponentWithType :: Typed.TypedTerm Syntax.VariableArityRecordComponent -> Typed.TypedTerm Syntax.UnannType -> Typed.TypedTerm Syntax.VariableArityRecordComponent
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
variableDeclarator :: Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm (Maybe Syntax.VariableInitializer) -> Typed.TypedTerm Syntax.VariableDeclarator
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
variableDeclaratorId :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm Syntax.VariableDeclaratorId
variableDeclaratorId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.VariableDeclaratorId
variableDeclaratorId2 :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm Syntax.VariableDeclaratorId
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
variableDeclaratorIdDims :: Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm (Maybe Syntax.Dims)
variableDeclaratorIdDims x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
        Core.projectionFieldName = (Core.Name "dims")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdIdentifier :: Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.Identifier
variableDeclaratorIdIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
        Core.projectionFieldName = (Core.Name "identifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dims field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdWithDims :: Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm (Maybe Syntax.Dims) -> Typed.TypedTerm Syntax.VariableDeclaratorId
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
variableDeclaratorIdWithIdentifier :: Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.VariableDeclaratorId
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
variableDeclaratorInitializer :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm (Maybe Syntax.VariableInitializer)
variableDeclaratorInitializer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
        Core.projectionFieldName = (Core.Name "initializer")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.VariableDeclarator
variableDeclaratorWithId :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm Syntax.VariableDeclaratorId -> Typed.TypedTerm Syntax.VariableDeclarator
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
variableDeclaratorWithInitializer :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm (Maybe Syntax.VariableInitializer) -> Typed.TypedTerm Syntax.VariableDeclarator
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
variableInitializerArrayInitializer :: Typed.TypedTerm Syntax.ArrayInitializer -> Typed.TypedTerm Syntax.VariableInitializer
variableInitializerArrayInitializer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayInitializer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.VariableInitializer
variableInitializerExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.VariableInitializer
variableInitializerExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.VariableModifier
variableModifierAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.VariableModifier
variableModifierAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.VariableModifier
variableModifierFinal :: Typed.TypedTerm Syntax.VariableModifier
variableModifierFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.WhileStatement
whileStatement :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.WhileStatement
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
whileStatementBody :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Statement
whileStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.WhileStatement
whileStatementCond :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm (Maybe Syntax.Expression)
whileStatementCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIf :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.WhileStatementNoShortIf
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
whileStatementNoShortIfBody :: Typed.TypedTerm Syntax.WhileStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf
whileStatementNoShortIfBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfCond :: Typed.TypedTerm Syntax.WhileStatementNoShortIf -> Typed.TypedTerm (Maybe Syntax.Expression)
whileStatementNoShortIfCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithBody :: Typed.TypedTerm Syntax.WhileStatementNoShortIf -> Typed.TypedTerm Syntax.StatementNoShortIf -> Typed.TypedTerm Syntax.WhileStatementNoShortIf
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
whileStatementNoShortIfWithCond :: Typed.TypedTerm Syntax.WhileStatementNoShortIf -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.WhileStatementNoShortIf
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
whileStatementWithBody :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.WhileStatement
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
whileStatementWithCond :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.WhileStatement
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
wildcard :: Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm (Maybe Syntax.WildcardBounds) -> Typed.TypedTerm Syntax.Wildcard
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
wildcardAnnotations :: Typed.TypedTerm Syntax.Wildcard -> Typed.TypedTerm [Syntax.Annotation]
wildcardAnnotations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the extends variant of hydra.java.syntax.WildcardBounds
wildcardBoundsExtends :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.WildcardBounds
wildcardBoundsExtends x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extends"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.WildcardBounds
wildcardBoundsSuper :: Typed.TypedTerm Syntax.ReferenceType -> Typed.TypedTerm Syntax.WildcardBounds
wildcardBoundsSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the wildcard field of hydra.java.syntax.Wildcard
wildcardWildcard :: Typed.TypedTerm Syntax.Wildcard -> Typed.TypedTerm (Maybe Syntax.WildcardBounds)
wildcardWildcard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
        Core.projectionFieldName = (Core.Name "wildcard")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.Wildcard
wildcardWithAnnotations :: Typed.TypedTerm Syntax.Wildcard -> Typed.TypedTerm [Syntax.Annotation] -> Typed.TypedTerm Syntax.Wildcard
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
wildcardWithWildcard :: Typed.TypedTerm Syntax.Wildcard -> Typed.TypedTerm (Maybe Syntax.WildcardBounds) -> Typed.TypedTerm Syntax.Wildcard
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
yieldStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.YieldStatement
yieldStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.YieldStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
