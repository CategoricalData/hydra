-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.java.syntax

module Hydra.Dsl.Java.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for the hydra.java.syntax.AdditionalBound wrapper
additionalBound :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.AdditionalBound
additionalBound x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AdditionalBound"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the minus variant of hydra.java.syntax.AdditiveExpression
additiveExpressionMinus :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionMinus x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the plus variant of hydra.java.syntax.AdditiveExpression
additiveExpressionPlus :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionPlus x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.AdditiveExpression
additiveExpressionUnary :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionUnary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_Binary :: Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AdditiveExpression_Binary
additiveExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryLhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryRhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
additiveExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AdditiveExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.AmbiguousName wrapper
ambiguousName :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.AmbiguousName
ambiguousName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AmbiguousName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.AndExpression wrapper
andExpression :: Phantoms.TTerm [Syntax.EqualityExpression] -> Phantoms.TTerm Syntax.AndExpression
andExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AndExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifier :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AnnotatedIdentifier
annotatedIdentifier annotations identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierAnnotations :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm [Syntax.Annotation]
annotatedIdentifierAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierIdentifier :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm Syntax.Identifier
annotatedIdentifierIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierWithAnnotations :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotatedIdentifier
annotatedIdentifierWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.AnnotatedIdentifier
annotatedIdentifierWithIdentifier :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AnnotatedIdentifier
annotatedIdentifierWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotatedIdentifier"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.AnnotationInterfaceBody wrapper
annotationInterfaceBody :: Phantoms.TTerm [Syntax.AnnotationInterfaceMemberDeclaration] -> Phantoms.TTerm Syntax.AnnotationInterfaceBody
annotationInterfaceBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclaration :: Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.AnnotationInterfaceBody -> Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclaration modifiers identifier body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationBody :: Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration -> Phantoms.TTerm Syntax.AnnotationInterfaceBody
annotationInterfaceDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationIdentifier :: Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
annotationInterfaceDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationModifiers :: Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier]
annotationInterfaceDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithBody :: Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration -> Phantoms.TTerm Syntax.AnnotationInterfaceBody -> Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithIdentifier :: Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithModifiers :: Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration
annotationInterfaceDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclaration :: Phantoms.TTerm [Syntax.AnnotationInterfaceElementModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclaration modifiers type_ identifier dims default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the default field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationDefault :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm (Maybe Syntax.DefaultValue)
annotationInterfaceElementDeclarationDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationDims :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm (Maybe Syntax.Dims)
annotationInterfaceElementDeclarationDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionField = (Core.Name "dims")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationIdentifier :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm Syntax.Identifier
annotationInterfaceElementDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationModifiers :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm [Syntax.AnnotationInterfaceElementModifier]
annotationInterfaceElementDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationType :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm Syntax.UnannType
annotationInterfaceElementDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the default field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithDefault :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the dims field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithDims :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithIdentifier :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithModifiers :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm [Syntax.AnnotationInterfaceElementModifier] -> Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithType :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration
annotationInterfaceElementDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementDeclaration"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the abstract variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAbstract :: Phantoms.TTerm Syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAbstract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the public variant of hydra.java.syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierPublic :: Phantoms.TTerm Syntax.AnnotationInterfaceElementModifier
annotationInterfaceElementModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotationInterface variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationAnnotationInterface :: Phantoms.TTerm Syntax.AnnotationInterfaceElementDeclaration -> Phantoms.TTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationAnnotationInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the constant variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationConstant :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationConstant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.AnnotationInterfaceMemberDeclaration
annotationInterfaceMemberDeclarationInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AnnotationInterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the marker variant of hydra.java.syntax.Annotation
annotationMarker :: Phantoms.TTerm Syntax.MarkerAnnotation -> Phantoms.TTerm Syntax.Annotation
annotationMarker x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "marker"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the normal variant of hydra.java.syntax.Annotation
annotationNormal :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm Syntax.Annotation
annotationNormal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the singleElement variant of hydra.java.syntax.Annotation
annotationSingleElement :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm Syntax.Annotation
annotationSingleElement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayAccess
arrayAccess :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ArrayAccess_Variant -> Phantoms.TTerm Syntax.ArrayAccess
arrayAccess expression variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.ArrayAccess
arrayAccessExpression :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm (Maybe Syntax.Expression)
arrayAccessExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ArrayAccess
arrayAccessVariant :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.ArrayAccess_Variant
arrayAccessVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
        Core.projectionField = (Core.Name "variant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.ArrayAccess
arrayAccessWithExpression :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ArrayAccess
arrayAccessWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
              Core.projectionField = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.ArrayAccess
arrayAccessWithVariant :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.ArrayAccess_Variant -> Phantoms.TTerm Syntax.ArrayAccess
arrayAccessWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the arrayCreationWithInitializer variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantArrayCreationWithInitializer :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer -> Phantoms.TTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantArrayCreationWithInitializer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayCreationWithInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.ArrayAccess_Variant
arrayAccess_VariantPrimary :: Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression -> Phantoms.TTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the withInitializer variant of hydra.java.syntax.ArrayCreationExpression
arrayCreationExpressionWithInitializer :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpression
arrayCreationExpressionWithInitializer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerClassOrInterface :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerClassOrInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerPrimitive :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer
arrayCreationExpressionWithInitializerPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterface type_ dims array =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm array)}]}))
-- | DSL accessor for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceArray :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ArrayInitializer
arrayCreationExpressionWithInitializer_ClassOrInterfaceArray x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionField = (Core.Name "array")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Phantoms.TTerm [Syntax.Dims]
arrayCreationExpressionWithInitializer_ClassOrInterfaceDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionField = (Core.Name "dims")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType
arrayCreationExpressionWithInitializer_ClassOrInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithArray :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithArray original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "array")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface
arrayCreationExpressionWithInitializer_ClassOrInterfaceWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "array")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_Primitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_Primitive type_ dims array =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm array)}]}))
-- | DSL accessor for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveArray :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Phantoms.TTerm Syntax.ArrayInitializer
arrayCreationExpressionWithInitializer_PrimitiveArray x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionField = (Core.Name "array")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Phantoms.TTerm [Syntax.Dims]
arrayCreationExpressionWithInitializer_PrimitiveDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionField = (Core.Name "dims")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
arrayCreationExpressionWithInitializer_PrimitiveType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the array field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithArray :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithArray original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionField = (Core.Name "array")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionWithInitializer_PrimitiveWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive"),
              Core.projectionField = (Core.Name "array")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the withoutInitializer variant of hydra.java.syntax.ArrayCreationExpression
arrayCreationExpressionWithoutInitializer :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpression
arrayCreationExpressionWithoutInitializer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerClassOrInterface :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerClassOrInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerPrimitive :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer
arrayCreationExpressionWithoutInitializerPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterface type_ dimExprs dims =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Phantoms.unTTerm dimExprs)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)}]}))
-- | DSL accessor for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Phantoms.TTerm [Syntax.DimExpr]
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDimExprs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionField = (Core.Name "dimExprs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Phantoms.TTerm (Maybe Syntax.Dims)
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionField = (Core.Name "dims")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDimExprs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface
arrayCreationExpressionWithoutInitializer_ClassOrInterfaceWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_ClassOrInterface"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_Primitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_Primitive type_ dimExprs dims =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Phantoms.unTTerm dimExprs)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)}]}))
-- | DSL accessor for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Phantoms.TTerm [Syntax.DimExpr]
arrayCreationExpressionWithoutInitializer_PrimitiveDimExprs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionField = (Core.Name "dimExprs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Phantoms.TTerm (Maybe Syntax.Dims)
arrayCreationExpressionWithoutInitializer_PrimitiveDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionField = (Core.Name "dims")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
arrayCreationExpressionWithoutInitializer_PrimitiveType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the dimExprs field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithDimExprs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionField = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithType :: Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.ArrayCreationExpressionWithoutInitializer_Primitive
arrayCreationExpressionWithoutInitializer_PrimitiveWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionField = (Core.Name "dimExprs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayCreationExpressionWithoutInitializer_Primitive"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.ArrayInitializer wrapper
arrayInitializer :: Phantoms.TTerm [[Syntax.VariableInitializer]] -> Phantoms.TTerm Syntax.ArrayInitializer
arrayInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ArrayInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.ArrayType
arrayType :: Phantoms.TTerm Syntax.Dims -> Phantoms.TTerm Syntax.ArrayType_Variant -> Phantoms.TTerm Syntax.ArrayType
arrayType dims variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))
-- | DSL accessor for the dims field of hydra.java.syntax.ArrayType
arrayTypeDims :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Dims
arrayTypeDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
        Core.projectionField = (Core.Name "dims")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ArrayType
arrayTypeVariant :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayTypeVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
        Core.projectionField = (Core.Name "variant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the dims field of hydra.java.syntax.ArrayType
arrayTypeWithDims :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Dims -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
              Core.projectionField = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.ArrayType
arrayTypeWithVariant :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.ArrayType_Variant -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ArrayType"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayType_VariantClassOrInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantPrimitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayType_VariantPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ArrayType_Variant
arrayType_VariantVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayType_VariantVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pair variant of hydra.java.syntax.AssertStatement
assertStatementPair :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.AssertStatement
assertStatementPair x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the single variant of hydra.java.syntax.AssertStatement
assertStatementSingle :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement
assertStatementSingle x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.AssertStatement_Pair
assertStatement_Pair :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement_Pair
assertStatement_Pair first second =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm second)}]}))
-- | DSL accessor for the first field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairFirst :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression
assertStatement_PairFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
        Core.projectionField = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the second field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairSecond :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression
assertStatement_PairSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
        Core.projectionField = (Core.Name "second")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the first field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairWithFirst :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement_Pair
assertStatement_PairWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
              Core.projectionField = (Core.Name "second")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the second field of hydra.java.syntax.AssertStatement_Pair
assertStatement_PairWithSecond :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement_Pair
assertStatement_PairWithSecond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.AssertStatement_Pair"),
              Core.projectionField = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.Assignment
assignment :: Phantoms.TTerm Syntax.LeftHandSide -> Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Assignment
assignment lhs op expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.Assignment
assignmentExpression :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.Expression
assignmentExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the assignment variant of hydra.java.syntax.AssignmentExpression
assignmentExpressionAssignment :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the conditional variant of hydra.java.syntax.AssignmentExpression
assignmentExpressionConditional :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionConditional x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the lhs field of hydra.java.syntax.Assignment
assignmentLhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.LeftHandSide
assignmentLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.java.syntax.Assignment
assignmentOp :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignmentOperator
assignmentOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the and variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorAnd :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorAnd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the div variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorDiv :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorDiv =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minus variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorMinus :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorMinus =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mod variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorMod :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorMod =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the or variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorOr :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorOr =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorPlus :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorPlus =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftLeft variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftLeft :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorShiftLeft =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftRight variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftRight :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorShiftRight =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the shiftRightZeroFill variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorShiftRightZeroFill :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorShiftRightZeroFill =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the simple variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorSimple :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorSimple =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the times variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorTimes :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorTimes =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the xor variant of hydra.java.syntax.AssignmentOperator
assignmentOperatorXor :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorXor =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the expression field of hydra.java.syntax.Assignment
assignmentWithExpression :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Assignment
assignmentWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the lhs field of hydra.java.syntax.Assignment
assignmentWithLhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.LeftHandSide -> Phantoms.TTerm Syntax.Assignment
assignmentWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.java.syntax.Assignment
assignmentWithOp :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.Assignment
assignmentWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.BasicForStatement
basicForStatement :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.BasicForStatement
basicForStatement cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.BasicForStatement
basicForStatementBody :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.Statement
basicForStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.BasicForStatement
basicForStatementCond :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.ForCond
basicForStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIf :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.BasicForStatementNoShortIf
basicForStatementNoShortIf cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfBody :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
basicForStatementNoShortIfBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfCond :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.ForCond
basicForStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithBody :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatementNoShortIf"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.BasicForStatement
basicForStatementWithBody :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.BasicForStatement
basicForStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.BasicForStatement
basicForStatementWithCond :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.BasicForStatement
basicForStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.BasicForStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.Block wrapper
block :: Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.Block
block x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Block"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the localClassOrInterface variant of hydra.java.syntax.BlockStatement
blockStatementLocalClassOrInterface :: Phantoms.TTerm Syntax.LocalClassOrInterfaceDeclaration -> Phantoms.TTerm Syntax.BlockStatement
blockStatementLocalClassOrInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localClassOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the localVariableDeclaration variant of hydra.java.syntax.BlockStatement
blockStatementLocalVariableDeclaration :: Phantoms.TTerm Syntax.LocalVariableDeclarationStatement -> Phantoms.TTerm Syntax.BlockStatement
blockStatementLocalVariableDeclaration x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariableDeclaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the statement variant of hydra.java.syntax.BlockStatement
blockStatementStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.BlockStatement
blockStatementStatement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the array variant of hydra.java.syntax.BooleanArray
booleanArrayArray :: Phantoms.TTerm Syntax.BooleanArray -> Phantoms.TTerm Syntax.BooleanArray
booleanArrayArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.BooleanArray
booleanArraySimple :: Phantoms.TTerm Syntax.BooleanArray
booleanArraySimple =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.BreakStatement wrapper
breakStatement :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.BreakStatement
breakStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.BreakStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.CaseConstant wrapper
caseConstant :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.CaseConstant
caseConstant x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.CaseConstant"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.CasePattern
casePattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm (Maybe Syntax.Guard) -> Phantoms.TTerm Syntax.CasePattern
casePattern pattern guard =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm guard)}]}))
-- | DSL accessor for the guard field of hydra.java.syntax.CasePattern
casePatternGuard :: Phantoms.TTerm Syntax.CasePattern -> Phantoms.TTerm (Maybe Syntax.Guard)
casePatternGuard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
        Core.projectionField = (Core.Name "guard")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pattern field of hydra.java.syntax.CasePattern
casePatternPattern :: Phantoms.TTerm Syntax.CasePattern -> Phantoms.TTerm Syntax.Pattern
casePatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the guard field of hydra.java.syntax.CasePattern
casePatternWithGuard :: Phantoms.TTerm Syntax.CasePattern -> Phantoms.TTerm (Maybe Syntax.Guard) -> Phantoms.TTerm Syntax.CasePattern
casePatternWithGuard original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.java.syntax.CasePattern
casePatternWithPattern :: Phantoms.TTerm Syntax.CasePattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.CasePattern
casePatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CasePattern"),
              Core.projectionField = (Core.Name "guard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the lambda variant of hydra.java.syntax.CastExpression
castExpressionLambda :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.CastExpression
castExpressionLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the notPlusMinus variant of hydra.java.syntax.CastExpression
castExpressionNotPlusMinus :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.CastExpression
castExpressionNotPlusMinus x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notPlusMinus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primitive variant of hydra.java.syntax.CastExpression
castExpressionPrimitive :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.CastExpression
castExpressionPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.CastExpression_Lambda
castExpression_Lambda :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CastExpression_Lambda
castExpression_Lambda refAndBounds expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm refAndBounds)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaExpression :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.LambdaExpression
castExpression_LambdaExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the refAndBounds field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_LambdaRefAndBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
        Core.projectionField = (Core.Name "refAndBounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaWithExpression :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CastExpression_Lambda
castExpression_LambdaWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
              Core.projectionField = (Core.Name "refAndBounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the refAndBounds field of hydra.java.syntax.CastExpression_Lambda
castExpression_LambdaWithRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.CastExpression_Lambda
castExpression_LambdaWithRefAndBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Lambda"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinus :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinus refAndBounds expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm refAndBounds)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusExpression :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.UnaryExpression
castExpression_NotPlusMinusExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the refAndBounds field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_NotPlusMinusRefAndBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionField = (Core.Name "refAndBounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithExpression :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
              Core.projectionField = (Core.Name "refAndBounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the refAndBounds field of hydra.java.syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithRefAndBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_NotPlusMinus"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CastExpression_Primitive
castExpression_Primitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_Primitive
castExpression_Primitive type_ expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveExpression :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.UnaryExpression
castExpression_PrimitiveExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveType :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
castExpression_PrimitiveType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveWithExpression :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_Primitive
castExpression_PrimitiveWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.CastExpression_Primitive
castExpression_PrimitiveWithType :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.CastExpression_Primitive
castExpression_PrimitiveWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_Primitive"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBounds :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_RefAndBounds type_ bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))
-- | DSL accessor for the bounds field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsBounds :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm [Syntax.AdditionalBound]
castExpression_RefAndBoundsBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionField = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsType :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.ReferenceType
castExpression_RefAndBoundsType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bounds field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithBounds :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithType :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CastExpression_RefAndBounds"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CatchClause
catchClause :: Phantoms.TTerm (Maybe Syntax.CatchFormalParameter) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CatchClause
catchClause parameter block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.CatchClause
catchClauseBlock :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm Syntax.Block
catchClauseBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameter field of hydra.java.syntax.CatchClause
catchClauseParameter :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm (Maybe Syntax.CatchFormalParameter)
catchClauseParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
        Core.projectionField = (Core.Name "parameter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.CatchClause
catchClauseWithBlock :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CatchClause
catchClauseWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
              Core.projectionField = (Core.Name "parameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the parameter field of hydra.java.syntax.CatchClause
catchClauseWithParameter :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm (Maybe Syntax.CatchFormalParameter) -> Phantoms.TTerm Syntax.CatchClause
catchClauseWithParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchClause"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CatchFormalParameter
catchFormalParameter :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameter modifiers type_ id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterId :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.VariableDeclaratorId
catchFormalParameterId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterModifiers :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm [Syntax.VariableModifier]
catchFormalParameterModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterType :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.CatchType
catchFormalParameterType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterWithId :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameterWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterWithModifiers :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameterWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.CatchFormalParameter
catchFormalParameterWithType :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameterWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CatchType
catchType :: Phantoms.TTerm Syntax.UnannClassType -> Phantoms.TTerm [Syntax.ClassType] -> Phantoms.TTerm Syntax.CatchType
catchType type_ types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))
-- | DSL accessor for the type field of hydra.java.syntax.CatchType
catchTypeType :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.UnannClassType
catchTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the types field of hydra.java.syntax.CatchType
catchTypeTypes :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm [Syntax.ClassType]
catchTypeTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
        Core.projectionField = (Core.Name "types")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the type field of hydra.java.syntax.CatchType
catchTypeWithType :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.UnannClassType -> Phantoms.TTerm Syntax.CatchType
catchTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the types field of hydra.java.syntax.CatchType
catchTypeWithTypes :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm [Syntax.ClassType] -> Phantoms.TTerm Syntax.CatchType
catchTypeWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CatchType"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.Catches wrapper
catches :: Phantoms.TTerm [Syntax.CatchClause] -> Phantoms.TTerm Syntax.Catches
catches x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Catches"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.ClassBody wrapper
classBody :: Phantoms.TTerm [Syntax.ClassBodyDeclarationWithComments] -> Phantoms.TTerm Syntax.ClassBody
classBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ClassBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the classMember variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationClassMember :: Phantoms.TTerm Syntax.ClassMemberDeclaration -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationClassMember x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classMember"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the constructorDeclaration variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationConstructorDeclaration :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationConstructorDeclaration x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructorDeclaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the instanceInitializer variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationInstanceInitializer :: Phantoms.TTerm Syntax.InstanceInitializer -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationInstanceInitializer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the staticInitializer variant of hydra.java.syntax.ClassBodyDeclaration
classBodyDeclarationStaticInitializer :: Phantoms.TTerm Syntax.StaticInitializer -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationStaticInitializer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithComments :: Phantoms.TTerm Syntax.ClassBodyDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithComments value comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsComments :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm (Maybe String)
classBodyDeclarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsValue :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationWithCommentsValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithComments :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the value field of hydra.java.syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithValue :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm Syntax.ClassBodyDeclaration -> Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassBodyDeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the enum variant of hydra.java.syntax.ClassDeclaration
classDeclarationEnum :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationEnum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the normal variant of hydra.java.syntax.ClassDeclaration
classDeclarationNormal :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationNormal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the record variant of hydra.java.syntax.ClassDeclaration
classDeclarationRecord :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpression :: Phantoms.TTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier) -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression
classInstanceCreationExpression qualifier expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionExpression :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
classInstanceCreationExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionQualifier :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier)
classInstanceCreationExpressionQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "qualifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithExpression :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "qualifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithQualifier :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier) -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the expression variant of hydra.java.syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierExpression :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.ClassLiteral
classLiteralBoolean :: Phantoms.TTerm Syntax.BooleanArray -> Phantoms.TTerm Syntax.ClassLiteral
classLiteralBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the numericType variant of hydra.java.syntax.ClassLiteral
classLiteralNumericType :: Phantoms.TTerm Syntax.NumericTypeArray -> Phantoms.TTerm Syntax.ClassLiteral
classLiteralNumericType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numericType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.ClassLiteral
classLiteralType :: Phantoms.TTerm Syntax.TypeNameArray -> Phantoms.TTerm Syntax.ClassLiteral
classLiteralType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the void variant of hydra.java.syntax.ClassLiteral
classLiteralVoid :: Phantoms.TTerm Syntax.ClassLiteral
classLiteralVoid =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the class variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the field variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationField :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "field"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the method variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationMethod :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationMethod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.ClassMemberDeclaration
classMemberDeclarationNone :: Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.ClassModifier
classModifierAbstract :: Phantoms.TTerm Syntax.ClassModifier
classModifierAbstract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ClassModifier
classModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ClassModifier
classModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.ClassModifier
classModifierFinal :: Phantoms.TTerm Syntax.ClassModifier
classModifierFinal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nonSealed variant of hydra.java.syntax.ClassModifier
classModifierNonSealed :: Phantoms.TTerm Syntax.ClassModifier
classModifierNonSealed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonSealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.ClassModifier
classModifierPrivate :: Phantoms.TTerm Syntax.ClassModifier
classModifierPrivate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.ClassModifier
classModifierProtected :: Phantoms.TTerm Syntax.ClassModifier
classModifierProtected =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ClassModifier
classModifierPublic :: Phantoms.TTerm Syntax.ClassModifier
classModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sealed variant of hydra.java.syntax.ClassModifier
classModifierSealed :: Phantoms.TTerm Syntax.ClassModifier
classModifierSealed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.ClassModifier
classModifierStatic :: Phantoms.TTerm Syntax.ClassModifier
classModifierStatic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.ClassModifier
classModifierStrictfp :: Phantoms.TTerm Syntax.ClassModifier
classModifierStrictfp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the class variant of hydra.java.syntax.ClassOrInterfaceType
classOrInterfaceTypeClass :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ClassOrInterfaceType
classOrInterfaceTypeClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.ClassOrInterfaceType
classOrInterfaceTypeInterface :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.ClassOrInterfaceType
classOrInterfaceTypeInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate :: Phantoms.TTerm [Syntax.AnnotatedIdentifier] -> Phantoms.TTerm (Maybe Syntax.TypeArgumentsOrDiamond) -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate identifiers typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm identifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))
-- | DSL accessor for the identifiers field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateIdentifiers :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm [Syntax.AnnotatedIdentifier]
classOrInterfaceTypeToInstantiateIdentifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionField = (Core.Name "identifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateTypeArguments :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm (Maybe Syntax.TypeArgumentsOrDiamond)
classOrInterfaceTypeToInstantiateTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifiers field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithIdentifiers :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm [Syntax.AnnotatedIdentifier] -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithIdentifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithTypeArguments :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm (Maybe Syntax.TypeArgumentsOrDiamond) -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassOrInterfaceTypeToInstantiate"),
              Core.projectionField = (Core.Name "identifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ClassType
classType :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassTypeQualifier -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ClassType
classType annotations qualifier identifier arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.ClassType
classTypeAnnotations :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.Annotation]
classTypeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the arguments field of hydra.java.syntax.ClassType
classTypeArguments :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.TypeArgument]
classTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ClassType
classTypeIdentifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.TypeIdentifier
classTypeIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ClassType
classTypeQualifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "qualifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the none variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierNone :: Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifierNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the package variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierPackage :: Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifierPackage x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "package"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the parent variant of hydra.java.syntax.ClassTypeQualifier
classTypeQualifierParent :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifierParent x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL updater for the annotations field of hydra.java.syntax.ClassType
classTypeWithAnnotations :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassType
classTypeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "qualifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the arguments field of hydra.java.syntax.ClassType
classTypeWithArguments :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ClassType
classTypeWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "qualifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.ClassType
classTypeWithIdentifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.ClassType
classTypeWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "qualifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.ClassType
classTypeWithQualifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ClassTypeQualifier -> Phantoms.TTerm Syntax.ClassType
classTypeWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclaration :: Phantoms.TTerm [Syntax.ConstructorModifier] -> Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm Syntax.CompactConstructorDeclaration
compactConstructorDeclaration modifiers name body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationBody :: Phantoms.TTerm Syntax.CompactConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorBody
compactConstructorDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationModifiers :: Phantoms.TTerm Syntax.CompactConstructorDeclaration -> Phantoms.TTerm [Syntax.ConstructorModifier]
compactConstructorDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationName :: Phantoms.TTerm Syntax.CompactConstructorDeclaration -> Phantoms.TTerm Syntax.SimpleTypeName
compactConstructorDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithBody :: Phantoms.TTerm Syntax.CompactConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm Syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithModifiers :: Phantoms.TTerm Syntax.CompactConstructorDeclaration -> Phantoms.TTerm [Syntax.ConstructorModifier] -> Phantoms.TTerm Syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.java.syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithName :: Phantoms.TTerm Syntax.CompactConstructorDeclaration -> Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm Syntax.CompactConstructorDeclaration
compactConstructorDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.CompactConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the modular variant of hydra.java.syntax.CompilationUnit
compilationUnitModular :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm Syntax.CompilationUnit
compilationUnitModular x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modular"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ordinary variant of hydra.java.syntax.CompilationUnit
compilationUnitOrdinary :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm Syntax.CompilationUnit
compilationUnitOrdinary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ConditionalAndExpression wrapper
conditionalAndExpression :: Phantoms.TTerm [Syntax.InclusiveOrExpression] -> Phantoms.TTerm Syntax.ConditionalAndExpression
conditionalAndExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConditionalAndExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionSimple :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ternaryCond variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionTernaryCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionTernaryCond x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryCond"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ternaryLambda variant of hydra.java.syntax.ConditionalExpression
conditionalExpressionTernaryLambda :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionTernaryLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCond :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCond cond ifTrue ifFalse =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Phantoms.unTTerm ifTrue)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Phantoms.unTTerm ifFalse)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalOrExpression
conditionalExpression_TernaryCondCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpression_TernaryCondIfFalse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionField = (Core.Name "ifFalse")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.Expression
conditionalExpression_TernaryCondIfTrue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionField = (Core.Name "ifTrue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfFalse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfTrue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambda :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambda cond ifTrue ifFalse =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Phantoms.unTTerm ifTrue)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Phantoms.unTTerm ifFalse)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.ConditionalOrExpression
conditionalExpression_TernaryLambdaCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.LambdaExpression
conditionalExpression_TernaryLambdaIfFalse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionField = (Core.Name "ifFalse")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.Expression
conditionalExpression_TernaryLambdaIfTrue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionField = (Core.Name "ifTrue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the ifFalse field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfFalse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifTrue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the ifTrue field of hydra.java.syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfTrue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifFalse")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.ConditionalOrExpression wrapper
conditionalOrExpression :: Phantoms.TTerm [Syntax.ConditionalAndExpression] -> Phantoms.TTerm Syntax.ConditionalOrExpression
conditionalOrExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConditionalOrExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.ConstantDeclaration
constantDeclaration :: Phantoms.TTerm [Syntax.ConstantModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclaration modifiers type_ variables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm variables)}]}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.ConstantDeclaration
constantDeclarationModifiers :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.ConstantModifier]
constantDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.ConstantDeclaration
constantDeclarationType :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.UnannType
constantDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variables field of hydra.java.syntax.ConstantDeclaration
constantDeclarationVariables :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator]
constantDeclarationVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
        Core.projectionField = (Core.Name "variables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ConstantDeclaration
constantDeclarationWithModifiers :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.ConstantModifier] -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.ConstantDeclaration
constantDeclarationWithType :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the variables field of hydra.java.syntax.ConstantDeclaration
constantDeclarationWithVariables :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclarationWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.ConstantExpression wrapper
constantExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConstantExpression
constantExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ConstantExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ConstantModifier
constantModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ConstantModifier
constantModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.ConstantModifier
constantModifierFinal :: Phantoms.TTerm Syntax.ConstantModifier
constantModifierFinal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ConstantModifier
constantModifierPublic :: Phantoms.TTerm Syntax.ConstantModifier
constantModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.ConstantModifier
constantModifierStatic :: Phantoms.TTerm Syntax.ConstantModifier
constantModifierStatic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.ConstructorBody
constructorBody :: Phantoms.TTerm (Maybe Syntax.ExplicitConstructorInvocation) -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.ConstructorBody
constructorBody invocation statements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Phantoms.unTTerm invocation)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)}]}))
-- | DSL accessor for the invocation field of hydra.java.syntax.ConstructorBody
constructorBodyInvocation :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm (Maybe Syntax.ExplicitConstructorInvocation)
constructorBodyInvocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
        Core.projectionField = (Core.Name "invocation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statements field of hydra.java.syntax.ConstructorBody
constructorBodyStatements :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm [Syntax.BlockStatement]
constructorBodyStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
        Core.projectionField = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the invocation field of hydra.java.syntax.ConstructorBody
constructorBodyWithInvocation :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm (Maybe Syntax.ExplicitConstructorInvocation) -> Phantoms.TTerm Syntax.ConstructorBody
constructorBodyWithInvocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
              Core.projectionField = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.java.syntax.ConstructorBody
constructorBodyWithStatements :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.ConstructorBody
constructorBodyWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorBody"),
              Core.projectionField = (Core.Name "invocation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ConstructorDeclaration
constructorDeclaration :: Phantoms.TTerm [Syntax.ConstructorModifier] -> Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclaration modifiers constructor throws body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Phantoms.unTTerm throws)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationBody :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorBody
constructorDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the constructor field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationConstructor :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclarationConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "constructor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationModifiers :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.ConstructorModifier]
constructorDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the throws field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationThrows :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm (Maybe Syntax.Throws)
constructorDeclarationThrows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "throws")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithBody :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "constructor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "throws")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the constructor field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithConstructor :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithConstructor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "throws")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithModifiers :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.ConstructorModifier] -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "constructor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "throws")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the throws field of hydra.java.syntax.ConstructorDeclaration
constructorDeclarationWithThrows :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithThrows original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "constructor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ConstructorDeclarator
constructorDeclarator :: Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclarator parameters name receiverParameter formalParameters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Phantoms.unTTerm receiverParameter)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Phantoms.unTTerm formalParameters)}]}))
-- | DSL accessor for the formalParameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorFormalParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.FormalParameter]
constructorDeclaratorFormalParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "formalParameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorName :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm Syntax.SimpleTypeName
constructorDeclaratorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.TypeParameter]
constructorDeclaratorParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the receiverParameter field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorReceiverParameter :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter)
constructorDeclaratorReceiverParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "receiverParameter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the formalParameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithFormalParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithFormalParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithName :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the receiverParameter field of hydra.java.syntax.ConstructorDeclarator
constructorDeclaratorWithReceiverParameter :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithReceiverParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ConstructorModifier
constructorModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the private variant of hydra.java.syntax.ConstructorModifier
constructorModifierPrivate :: Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierPrivate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.ConstructorModifier
constructorModifierProtected :: Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierProtected =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.ConstructorModifier
constructorModifierPublic :: Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.ContinueStatement wrapper
continueStatement :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ContinueStatement
continueStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ContinueStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.DefaultValue wrapper
defaultValue :: Phantoms.TTerm Syntax.ElementValue -> Phantoms.TTerm Syntax.DefaultValue
defaultValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.DefaultValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.DimExpr
dimExpr :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DimExpr
dimExpr annotations expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.DimExpr
dimExprAnnotations :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm [Syntax.Annotation]
dimExprAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.DimExpr
dimExprExpression :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm (Maybe Syntax.Expression)
dimExprExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.DimExpr
dimExprWithAnnotations :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DimExpr
dimExprWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expression field of hydra.java.syntax.DimExpr
dimExprWithExpression :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DimExpr
dimExprWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DimExpr"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.Dims wrapper
dims :: Phantoms.TTerm [[Syntax.Annotation]] -> Phantoms.TTerm Syntax.Dims
dims x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Dims"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.DoStatement
doStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DoStatement
doStatement body cond =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.DoStatement
doStatementBody :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Statement
doStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.DoStatement
doStatementCond :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Expression
doStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.DoStatement
doStatementWithBody :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.DoStatement
doStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the cond field of hydra.java.syntax.DoStatement
doStatementWithCond :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DoStatement
doStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.DoStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the annotation variant of hydra.java.syntax.ElementValue
elementValueAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ElementValue
elementValueAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ElementValueArrayInitializer wrapper
elementValueArrayInitializer :: Phantoms.TTerm [Syntax.ElementValue] -> Phantoms.TTerm Syntax.ElementValueArrayInitializer
elementValueArrayInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ElementValueArrayInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the conditionalExpression variant of hydra.java.syntax.ElementValue
elementValueConditionalExpression :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.ElementValue
elementValueConditionalExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditionalExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the elementValueArrayInitializer variant of hydra.java.syntax.ElementValue
elementValueElementValueArrayInitializer :: Phantoms.TTerm Syntax.ElementValueArrayInitializer -> Phantoms.TTerm Syntax.ElementValue
elementValueElementValueArrayInitializer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elementValueArrayInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ElementValuePair
elementValuePair :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ElementValue -> Phantoms.TTerm Syntax.ElementValuePair
elementValuePair key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the key field of hydra.java.syntax.ElementValuePair
elementValuePairKey :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.Identifier
elementValuePairKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.ElementValuePair
elementValuePairValue :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.ElementValue
elementValuePairValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the key field of hydra.java.syntax.ElementValuePair
elementValuePairWithKey :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ElementValuePair
elementValuePairWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.java.syntax.ElementValuePair
elementValuePairWithValue :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.ElementValue -> Phantoms.TTerm Syntax.ElementValuePair
elementValuePairWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ElementValuePair"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.EnhancedForCond
enhancedForCond :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCond declaration expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Phantoms.unTTerm declaration)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the declaration field of hydra.java.syntax.EnhancedForCond
enhancedForCondDeclaration :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.LocalVariableDeclaration
enhancedForCondDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
        Core.projectionField = (Core.Name "declaration")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.EnhancedForCond
enhancedForCondExpression :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.Expression
enhancedForCondExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the declaration field of hydra.java.syntax.EnhancedForCond
enhancedForCondWithDeclaration :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCondWithDeclaration original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expression field of hydra.java.syntax.EnhancedForCond
enhancedForCondWithExpression :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCondWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "declaration")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.EnhancedForStatement
enhancedForStatement :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.EnhancedForStatement
enhancedForStatement cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementBody :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.Statement
enhancedForStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementCond :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIf :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIf cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfBody :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
enhancedForStatementNoShortIfBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfCond :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithBody :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatementNoShortIf"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementWithBody :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.EnhancedForStatement
enhancedForStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.EnhancedForStatement
enhancedForStatementWithCond :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.EnhancedForStatement
enhancedForStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnhancedForStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.EnumBody wrapper
enumBody :: Phantoms.TTerm [Syntax.EnumBody_Element] -> Phantoms.TTerm Syntax.EnumBody
enumBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.EnumBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.EnumBody_Element
enumBody_Element :: Phantoms.TTerm [Syntax.EnumConstant] -> Phantoms.TTerm [Syntax.ClassBodyDeclaration] -> Phantoms.TTerm Syntax.EnumBody_Element
enumBody_Element constants bodyDeclarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Phantoms.unTTerm constants)},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Phantoms.unTTerm bodyDeclarations)}]}))
-- | DSL accessor for the bodyDeclarations field of hydra.java.syntax.EnumBody_Element
enumBody_ElementBodyDeclarations :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.ClassBodyDeclaration]
enumBody_ElementBodyDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
        Core.projectionField = (Core.Name "bodyDeclarations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the constants field of hydra.java.syntax.EnumBody_Element
enumBody_ElementConstants :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.EnumConstant]
enumBody_ElementConstants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
        Core.projectionField = (Core.Name "constants")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bodyDeclarations field of hydra.java.syntax.EnumBody_Element
enumBody_ElementWithBodyDeclarations :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.ClassBodyDeclaration] -> Phantoms.TTerm Syntax.EnumBody_Element
enumBody_ElementWithBodyDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
              Core.projectionField = (Core.Name "constants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the constants field of hydra.java.syntax.EnumBody_Element
enumBody_ElementWithConstants :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.EnumConstant] -> Phantoms.TTerm Syntax.EnumBody_Element
enumBody_ElementWithConstants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumBody_Element"),
              Core.projectionField = (Core.Name "bodyDeclarations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.EnumConstant
enumConstant :: Phantoms.TTerm [Syntax.EnumConstantModifier] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe [Syntax.Expression]) -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.EnumConstant
enumConstant modifiers identifier arguments body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.EnumConstant
enumConstantArguments :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm (Maybe [Syntax.Expression])
enumConstantArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.EnumConstant
enumConstantBody :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm (Maybe Syntax.ClassBody)
enumConstantBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.EnumConstant
enumConstantIdentifier :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm Syntax.Identifier
enumConstantIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.EnumConstantModifier wrapper
enumConstantModifier :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.EnumConstantModifier
enumConstantModifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.EnumConstantModifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.EnumConstant
enumConstantModifiers :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm [Syntax.EnumConstantModifier]
enumConstantModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.EnumConstant
enumConstantWithArguments :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm (Maybe [Syntax.Expression]) -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.EnumConstant
enumConstantWithBody :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.EnumConstant
enumConstantWithIdentifier :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.EnumConstant
enumConstantWithModifiers :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm [Syntax.EnumConstantModifier] -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.EnumDeclaration
enumDeclaration :: Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.EnumBody -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclaration modifiers identifier implements body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm implements)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.EnumDeclaration
enumDeclarationBody :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.EnumBody
enumDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.EnumDeclaration
enumDeclarationIdentifier :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
enumDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.EnumDeclaration
enumDeclarationImplements :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.InterfaceType]
enumDeclarationImplements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "implements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.EnumDeclaration
enumDeclarationModifiers :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.ClassModifier]
enumDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithBody :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.EnumBody -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithIdentifier :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the implements field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithImplements :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithImplements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.EnumDeclaration
enumDeclarationWithModifiers :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the equal variant of hydra.java.syntax.EqualityExpression
equalityExpressionEqual :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionEqual x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the notEqual variant of hydra.java.syntax.EqualityExpression
equalityExpressionNotEqual :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionNotEqual x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.EqualityExpression
equalityExpressionUnary :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionUnary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.EqualityExpression_Binary
equalityExpression_Binary :: Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualityExpression_Binary
equalityExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryLhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryRhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.RelationalExpression
equalityExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.EqualityExpression_Binary
equalityExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.EqualityExpression_Binary
equalityExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualityExpression_Binary
equalityExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.EqualityExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the class variant of hydra.java.syntax.ExceptionType
exceptionTypeClass :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ExceptionType
exceptionTypeClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ExceptionType
exceptionTypeVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.ExceptionType
exceptionTypeVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ExclusiveOrExpression wrapper
exclusiveOrExpression :: Phantoms.TTerm [Syntax.AndExpression] -> Phantoms.TTerm Syntax.ExclusiveOrExpression
exclusiveOrExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ExclusiveOrExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocation :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocation typeArguments arguments variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.Expression]
explicitConstructorInvocationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationTypeArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.TypeArgument]
explicitConstructorInvocationTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationVariant :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocationVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionField = (Core.Name "variant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithTypeArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithVariant :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the primary variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantSuper :: Phantoms.TTerm (Maybe Syntax.ExpressionName) -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantSuper x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the this variant of hydra.java.syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantThis :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantThis =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the assignment variant of hydra.java.syntax.Expression
expressionAssignment :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression
expressionAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lambda variant of hydra.java.syntax.Expression
expressionLambda :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ExpressionName
expressionName :: Phantoms.TTerm (Maybe Syntax.AmbiguousName) -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ExpressionName
expressionName qualifier identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ExpressionName
expressionNameIdentifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.Identifier
expressionNameIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.ExpressionName
expressionNameQualifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm (Maybe Syntax.AmbiguousName)
expressionNameQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
        Core.projectionField = (Core.Name "qualifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.ExpressionName
expressionNameWithIdentifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ExpressionName
expressionNameWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
              Core.projectionField = (Core.Name "qualifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.ExpressionName
expressionNameWithQualifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm (Maybe Syntax.AmbiguousName) -> Phantoms.TTerm Syntax.ExpressionName
expressionNameWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ExpressionName"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.ExpressionStatement wrapper
expressionStatement :: Phantoms.TTerm Syntax.StatementExpression -> Phantoms.TTerm Syntax.ExpressionStatement
expressionStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ExpressionStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.FieldAccess
fieldAccess :: Phantoms.TTerm Syntax.FieldAccess_Qualifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.FieldAccess
fieldAccess qualifier identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.FieldAccess
fieldAccessIdentifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Identifier
fieldAccessIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.FieldAccess
fieldAccessQualifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccessQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
        Core.projectionField = (Core.Name "qualifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.FieldAccess
fieldAccessWithIdentifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "qualifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.FieldAccess
fieldAccessWithQualifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.FieldAccess_Qualifier -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the primary variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierSuper :: Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierSuper =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typed variant of hydra.java.syntax.FieldAccess_Qualifier
fieldAccess_QualifierTyped :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierTyped x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.FieldDeclaration
fieldDeclaration :: Phantoms.TTerm [Syntax.FieldModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclaration modifiers unannType variableDeclarators =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Phantoms.unTTerm unannType)},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Phantoms.unTTerm variableDeclarators)}]}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.FieldDeclaration
fieldDeclarationModifiers :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.FieldModifier]
fieldDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the unannType field of hydra.java.syntax.FieldDeclaration
fieldDeclarationUnannType :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm Syntax.UnannType
fieldDeclarationUnannType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionField = (Core.Name "unannType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variableDeclarators field of hydra.java.syntax.FieldDeclaration
fieldDeclarationVariableDeclarators :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator]
fieldDeclarationVariableDeclarators x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
        Core.projectionField = (Core.Name "variableDeclarators")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.FieldDeclaration
fieldDeclarationWithModifiers :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.FieldModifier] -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "unannType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "variableDeclarators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the unannType field of hydra.java.syntax.FieldDeclaration
fieldDeclarationWithUnannType :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclarationWithUnannType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "variableDeclarators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the variableDeclarators field of hydra.java.syntax.FieldDeclaration
fieldDeclarationWithVariableDeclarators :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclarationWithVariableDeclarators original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "unannType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the annotation variant of hydra.java.syntax.FieldModifier
fieldModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.FieldModifier
fieldModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.FieldModifier
fieldModifierFinal :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierFinal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.FieldModifier
fieldModifierPrivate :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierPrivate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.FieldModifier
fieldModifierProtected :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierProtected =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.FieldModifier
fieldModifierPublic :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.FieldModifier
fieldModifierStatic :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierStatic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transient variant of hydra.java.syntax.FieldModifier
fieldModifierTransient :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierTransient =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transient"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the volatile variant of hydra.java.syntax.FieldModifier
fieldModifierVolatile :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierVolatile =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "volatile"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.Finally wrapper
finally :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Finally
finally x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Finally"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.FloatingPointLiteral wrapper
floatingPointLiteral :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatingPointLiteral
floatingPointLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.FloatingPointLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the double variant of hydra.java.syntax.FloatingPointType
floatingPointTypeDouble :: Phantoms.TTerm Syntax.FloatingPointType
floatingPointTypeDouble =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float variant of hydra.java.syntax.FloatingPointType
floatingPointTypeFloat :: Phantoms.TTerm Syntax.FloatingPointType
floatingPointTypeFloat =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.ForCond
forCond :: Phantoms.TTerm (Maybe Syntax.ForInit) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.ForUpdate) -> Phantoms.TTerm Syntax.ForCond
forCond init cond update =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Phantoms.unTTerm update)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.ForCond
forCondCond :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.Expression)
forCondCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the init field of hydra.java.syntax.ForCond
forCondInit :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForInit)
forCondInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionField = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the update field of hydra.java.syntax.ForCond
forCondUpdate :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForUpdate)
forCondUpdate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
        Core.projectionField = (Core.Name "update")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.ForCond
forCondWithCond :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ForCond
forCondWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "update")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the init field of hydra.java.syntax.ForCond
forCondWithInit :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForInit) -> Phantoms.TTerm Syntax.ForCond
forCondWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "update")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the update field of hydra.java.syntax.ForCond
forCondWithUpdate :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForUpdate) -> Phantoms.TTerm Syntax.ForCond
forCondWithUpdate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the localVariable variant of hydra.java.syntax.ForInit
forInitLocalVariable :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.ForInit
forInitLocalVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the statements variant of hydra.java.syntax.ForInit
forInitStatements :: Phantoms.TTerm [Syntax.StatementExpression] -> Phantoms.TTerm Syntax.ForInit
forInitStatements x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statements"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the basic variant of hydra.java.syntax.ForStatement
forStatementBasic :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.ForStatement
forStatementBasic x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the enhanced variant of hydra.java.syntax.ForStatement
forStatementEnhanced :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.ForStatement
forStatementEnhanced x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the basic variant of hydra.java.syntax.ForStatementNoShortIf
forStatementNoShortIfBasic :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.ForStatementNoShortIf
forStatementNoShortIfBasic x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the enhanced variant of hydra.java.syntax.ForStatementNoShortIf
forStatementNoShortIfEnhanced :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.ForStatementNoShortIf
forStatementNoShortIfEnhanced x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ForUpdate wrapper
forUpdate :: Phantoms.TTerm [Syntax.StatementExpression] -> Phantoms.TTerm Syntax.ForUpdate
forUpdate x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ForUpdate"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.FormalParameter
formalParameterSimple :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.FormalParameter
formalParameterSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.FormalParameter
formalParameterVariableArity :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.FormalParameter
formalParameterVariableArity x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.FormalParameter_Simple
formalParameter_Simple :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_Simple modifiers type_ id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleId :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.VariableDeclaratorId
formalParameter_SimpleId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleModifiers :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm [Syntax.VariableModifier]
formalParameter_SimpleModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleType :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.UnannType
formalParameter_SimpleType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleWithId :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_SimpleWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleWithModifiers :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_SimpleWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.FormalParameter_Simple
formalParameter_SimpleWithType :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_SimpleWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.Guard wrapper
guard :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Guard
guard x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Guard"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.Identifier wrapper
identifier :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Identifier
identifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Identifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.IfThenElseStatement
ifThenElseStatement :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatement cond then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementCond :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
ifThenElseStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementElse :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.Statement
ifThenElseStatementElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIf :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIf cond then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))
-- | DSL accessor for the cond field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfCond :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression)
ifThenElseStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfElse :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
ifThenElseStatementNoShortIfElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the then field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfThen :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
ifThenElseStatementNoShortIfThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionField = (Core.Name "then")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithElse :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the then field of hydra.java.syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithThen :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the then field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementThen :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.StatementNoShortIf
ifThenElseStatementThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
        Core.projectionField = (Core.Name "then")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cond field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementWithCond :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementWithElse :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatementWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the then field of hydra.java.syntax.IfThenElseStatement
ifThenElseStatementWithThen :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatementWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.IfThenStatement
ifThenStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenStatement
ifThenStatement expression statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.IfThenStatement
ifThenStatementExpression :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Expression
ifThenStatementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statement field of hydra.java.syntax.IfThenStatement
ifThenStatementStatement :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Statement
ifThenStatementStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
        Core.projectionField = (Core.Name "statement")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.IfThenStatement
ifThenStatementWithExpression :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfThenStatement
ifThenStatementWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
              Core.projectionField = (Core.Name "statement")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.java.syntax.IfThenStatement
ifThenStatementWithStatement :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenStatement
ifThenStatementWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.IfThenStatement"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the singleStaticImport variant of hydra.java.syntax.ImportDeclaration
importDeclarationSingleStaticImport :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationSingleStaticImport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleStaticImport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the singleType variant of hydra.java.syntax.ImportDeclaration
importDeclarationSingleType :: Phantoms.TTerm Syntax.SingleTypeImportDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationSingleType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the staticImportOnDemand variant of hydra.java.syntax.ImportDeclaration
importDeclarationStaticImportOnDemand :: Phantoms.TTerm Syntax.StaticImportOnDemandDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationStaticImportOnDemand x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticImportOnDemand"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeImportOnDemand variant of hydra.java.syntax.ImportDeclaration
importDeclarationTypeImportOnDemand :: Phantoms.TTerm Syntax.TypeImportOnDemandDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationTypeImportOnDemand x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeImportOnDemand"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.InclusiveOrExpression wrapper
inclusiveOrExpression :: Phantoms.TTerm [Syntax.ExclusiveOrExpression] -> Phantoms.TTerm Syntax.InclusiveOrExpression
inclusiveOrExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InclusiveOrExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.InstanceInitializer wrapper
instanceInitializer :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.InstanceInitializer
instanceInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InstanceInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.InstanceofExpression
instanceofExpression :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.InstanceofExpression_Rhs -> Phantoms.TTerm Syntax.InstanceofExpression
instanceofExpression lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionLhs :: Phantoms.TTerm Syntax.InstanceofExpression -> Phantoms.TTerm Syntax.RelationalExpression
instanceofExpressionLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionRhs :: Phantoms.TTerm Syntax.InstanceofExpression -> Phantoms.TTerm Syntax.InstanceofExpression_Rhs
instanceofExpressionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionWithLhs :: Phantoms.TTerm Syntax.InstanceofExpression -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.InstanceofExpression
instanceofExpressionWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.InstanceofExpression
instanceofExpressionWithRhs :: Phantoms.TTerm Syntax.InstanceofExpression -> Phantoms.TTerm Syntax.InstanceofExpression_Rhs -> Phantoms.TTerm Syntax.InstanceofExpression
instanceofExpressionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the pattern variant of hydra.java.syntax.InstanceofExpression_Rhs
instanceofExpression_RhsPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.InstanceofExpression_Rhs
instanceofExpression_RhsPattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression_Rhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the referenceType variant of hydra.java.syntax.InstanceofExpression_Rhs
instanceofExpression_RhsReferenceType :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.InstanceofExpression_Rhs
instanceofExpression_RhsReferenceType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InstanceofExpression_Rhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "referenceType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.IntegerLiteral wrapper
integerLiteral :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.IntegerLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the byte variant of hydra.java.syntax.IntegralType
integralTypeByte :: Phantoms.TTerm Syntax.IntegralType
integralTypeByte =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the char variant of hydra.java.syntax.IntegralType
integralTypeChar :: Phantoms.TTerm Syntax.IntegralType
integralTypeChar =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int variant of hydra.java.syntax.IntegralType
integralTypeInt :: Phantoms.TTerm Syntax.IntegralType
integralTypeInt =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the long variant of hydra.java.syntax.IntegralType
integralTypeLong :: Phantoms.TTerm Syntax.IntegralType
integralTypeLong =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the short variant of hydra.java.syntax.IntegralType
integralTypeShort :: Phantoms.TTerm Syntax.IntegralType
integralTypeShort =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.InterfaceBody wrapper
interfaceBody :: Phantoms.TTerm [Syntax.InterfaceMemberDeclarationWithComments] -> Phantoms.TTerm Syntax.InterfaceBody
interfaceBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InterfaceBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the annotationInterface variant of hydra.java.syntax.InterfaceDeclaration
interfaceDeclarationAnnotationInterface :: Phantoms.TTerm Syntax.AnnotationInterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceDeclaration
interfaceDeclarationAnnotationInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the normalInterface variant of hydra.java.syntax.InterfaceDeclaration
interfaceDeclarationNormalInterface :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceDeclaration
interfaceDeclarationNormalInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the constant variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationConstant :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationConstant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interfaceMethod variant of hydra.java.syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterfaceMethod :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterfaceMethod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interfaceMethod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithComments :: Phantoms.TTerm Syntax.InterfaceMemberDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithComments value comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsComments :: Phantoms.TTerm Syntax.InterfaceMemberDeclarationWithComments -> Phantoms.TTerm (Maybe String)
interfaceMemberDeclarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsValue :: Phantoms.TTerm Syntax.InterfaceMemberDeclarationWithComments -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationWithCommentsValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsWithComments :: Phantoms.TTerm Syntax.InterfaceMemberDeclarationWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the value field of hydra.java.syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsWithValue :: Phantoms.TTerm Syntax.InterfaceMemberDeclarationWithComments -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclarationWithComments
interfaceMemberDeclarationWithCommentsWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMemberDeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclaration :: Phantoms.TTerm [Syntax.InterfaceMethodModifier] -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclaration modifiers header body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationBody :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodBody
interfaceMethodDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationHeader :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader
interfaceMethodDeclarationHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionField = (Core.Name "header")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationModifiers :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm [Syntax.InterfaceMethodModifier]
interfaceMethodDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithBody :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithHeader :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithModifiers :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm [Syntax.InterfaceMethodModifier] -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the abstract variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierAbstract :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierAbstract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the default variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierDefault :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierDefault =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierPrivate :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierPrivate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierPublic :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierStatic :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierStatic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.InterfaceMethodModifier
interfaceMethodModifierStrictfp :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierStrictfp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.InterfaceModifier
interfaceModifierAbstract :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierAbstract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.InterfaceModifier
interfaceModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nonSealed variant of hydra.java.syntax.InterfaceModifier
interfaceModifierNonSealed :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierNonSealed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonSealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.InterfaceModifier
interfaceModifierPrivate :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierPrivate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.InterfaceModifier
interfaceModifierProtected :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierProtected =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.InterfaceModifier
interfaceModifierPublic :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sealed variant of hydra.java.syntax.InterfaceModifier
interfaceModifierSealed :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierSealed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.InterfaceModifier
interfaceModifierStatic :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierStatic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.InterfaceModifier
interfaceModifierStrictfp :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierStrictfp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.InterfaceType wrapper
interfaceType :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.InterfaceType
interfaceType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.InterfaceType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.LabeledStatement
labeledStatement :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatement identifier statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.LabeledStatement
labeledStatementIdentifier :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Identifier
labeledStatementIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIf :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.LabeledStatementNoShortIf
labeledStatementNoShortIf identifier statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfIdentifier :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.Identifier
labeledStatementNoShortIfIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statement field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfStatement :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
labeledStatementNoShortIfStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionField = (Core.Name "statement")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithIdentifier :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
              Core.projectionField = (Core.Name "statement")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.java.syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithStatement :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatementNoShortIf"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the statement field of hydra.java.syntax.LabeledStatement
labeledStatementStatement :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
labeledStatementStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "statement")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.LabeledStatement
labeledStatementWithIdentifier :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "statement")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.java.syntax.LabeledStatement
labeledStatementWithStatement :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the block variant of hydra.java.syntax.LambdaBody
lambdaBodyBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.LambdaBody
lambdaBodyBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.LambdaBody
lambdaBodyExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaBody
lambdaBodyExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LambdaExpression
lambdaExpression :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.LambdaBody -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpression parameters body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.LambdaExpression
lambdaExpressionBody :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaBody
lambdaExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.LambdaExpression
lambdaExpressionParameters :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaParameters
lambdaExpressionParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.LambdaExpression
lambdaExpressionWithBody :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaBody -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.LambdaExpression
lambdaExpressionWithParameters :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the normal variant of hydra.java.syntax.LambdaParameter
lambdaParameterNormal :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.LambdaParameter
lambdaParameterNormal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.LambdaParameterType
lambdaParameterTypeType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.LambdaParameterType
lambdaParameterTypeType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the var variant of hydra.java.syntax.LambdaParameterType
lambdaParameterTypeVar :: Phantoms.TTerm Syntax.LambdaParameterType
lambdaParameterTypeVar =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.LambdaParameter
lambdaParameterVariableArity :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.LambdaParameter
lambdaParameterVariableArity x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_Normal :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LambdaParameterType -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_Normal modifiers type_ id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalId :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.VariableDeclaratorId
lambdaParameter_NormalId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalModifiers :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm [Syntax.VariableModifier]
lambdaParameter_NormalModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalType :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.LambdaParameterType
lambdaParameter_NormalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalWithId :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_NormalWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalWithModifiers :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_NormalWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.LambdaParameter_Normal
lambdaParameter_NormalWithType :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.LambdaParameterType -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_NormalWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the single variant of hydra.java.syntax.LambdaParameters
lambdaParametersSingle :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersSingle x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.java.syntax.LambdaParameters
lambdaParametersTuple :: Phantoms.TTerm [Syntax.LambdaParameters] -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the arrayAccess variant of hydra.java.syntax.LeftHandSide
leftHandSideArrayAccess :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.LeftHandSide
leftHandSideArrayAccess x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expressionName variant of hydra.java.syntax.LeftHandSide
leftHandSideExpressionName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.LeftHandSide
leftHandSideExpressionName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.LeftHandSide
leftHandSideFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.LeftHandSide
leftHandSideFieldAccess x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.Literal
literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the character variant of hydra.java.syntax.Literal
literalCharacter :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Literal
literalCharacter x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the floatingPoint variant of hydra.java.syntax.Literal
literalFloatingPoint :: Phantoms.TTerm Syntax.FloatingPointLiteral -> Phantoms.TTerm Syntax.Literal
literalFloatingPoint x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integer variant of hydra.java.syntax.Literal
literalInteger :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the null variant of hydra.java.syntax.Literal
literalNull :: Phantoms.TTerm Syntax.Literal
literalNull =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the string variant of hydra.java.syntax.Literal
literalString :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the textBlock variant of hydra.java.syntax.Literal
literalTextBlock :: Phantoms.TTerm Syntax.TextBlock -> Phantoms.TTerm Syntax.Literal
literalTextBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "textBlock"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the class variant of hydra.java.syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the normalInterface variant of hydra.java.syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationNormalInterface :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.LocalClassOrInterfaceDeclaration
localClassOrInterfaceDeclarationNormalInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.LocalVariableDeclaration
localVariableDeclaration :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclaration modifiers type_ declarators =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Phantoms.unTTerm declarators)}]}))
-- | DSL accessor for the declarators field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationDeclarators :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator]
localVariableDeclarationDeclarators x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionField = (Core.Name "declarators")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationModifiers :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableModifier]
localVariableDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.LocalVariableDeclarationStatement wrapper
localVariableDeclarationStatement :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.LocalVariableDeclarationStatement
localVariableDeclarationStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclarationStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationType :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.LocalVariableType
localVariableDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the declarators field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationWithDeclarators :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclarationWithDeclarators original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationWithModifiers :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "declarators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.LocalVariableDeclaration
localVariableDeclarationWithType :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "declarators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the type variant of hydra.java.syntax.LocalVariableType
localVariableTypeType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.LocalVariableType
localVariableTypeType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the var variant of hydra.java.syntax.LocalVariableType
localVariableTypeVar :: Phantoms.TTerm Syntax.LocalVariableType
localVariableTypeVar =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.MarkerAnnotation wrapper
markerAnnotation :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.MarkerAnnotation
markerAnnotation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MarkerAnnotation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the block variant of hydra.java.syntax.MethodBody
methodBodyBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.MethodBody
methodBodyBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.MethodBody
methodBodyNone :: Phantoms.TTerm Syntax.MethodBody
methodBodyNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.MethodDeclaration
methodDeclaration :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.MethodModifier] -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclaration annotations modifiers header body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.MethodDeclaration
methodDeclarationAnnotations :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.Annotation]
methodDeclarationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.MethodDeclaration
methodDeclarationBody :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodBody
methodDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.MethodDeclaration
methodDeclarationHeader :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader
methodDeclarationHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "header")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.MethodDeclaration
methodDeclarationModifiers :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.MethodModifier]
methodDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithAnnotations :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithBody :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithHeader :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.MethodDeclaration
methodDeclarationWithModifiers :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.MethodModifier] -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodDeclarator
methodDeclarator :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclarator identifier receiverParameter formalParameters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Phantoms.unTTerm receiverParameter)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Phantoms.unTTerm formalParameters)}]}))
-- | DSL accessor for the formalParameters field of hydra.java.syntax.MethodDeclarator
methodDeclaratorFormalParameters :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm [Syntax.FormalParameter]
methodDeclaratorFormalParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionField = (Core.Name "formalParameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodDeclarator
methodDeclaratorIdentifier :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm Syntax.Identifier
methodDeclaratorIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the receiverParameter field of hydra.java.syntax.MethodDeclarator
methodDeclaratorReceiverParameter :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter)
methodDeclaratorReceiverParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
        Core.projectionField = (Core.Name "receiverParameter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the formalParameters field of hydra.java.syntax.MethodDeclarator
methodDeclaratorWithFormalParameters :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclaratorWithFormalParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodDeclarator
methodDeclaratorWithIdentifier :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclaratorWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the receiverParameter field of hydra.java.syntax.MethodDeclarator
methodDeclaratorWithReceiverParameter :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclaratorWithReceiverParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodHeader
methodHeader :: Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.Result -> Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.MethodHeader
methodHeader parameters result declarator throws =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm result)},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Phantoms.unTTerm declarator)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Phantoms.unTTerm throws)}]}))
-- | DSL accessor for the declarator field of hydra.java.syntax.MethodHeader
methodHeaderDeclarator :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodDeclarator
methodHeaderDeclarator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "declarator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.MethodHeader
methodHeaderParameters :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm [Syntax.TypeParameter]
methodHeaderParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the result field of hydra.java.syntax.MethodHeader
methodHeaderResult :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.Result
methodHeaderResult x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "result")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the throws field of hydra.java.syntax.MethodHeader
methodHeaderThrows :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm (Maybe Syntax.Throws)
methodHeaderThrows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "throws")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the declarator field of hydra.java.syntax.MethodHeader
methodHeaderWithDeclarator :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithDeclarator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "result")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "throws")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.MethodHeader
methodHeaderWithParameters :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "result")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "declarator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "throws")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the result field of hydra.java.syntax.MethodHeader
methodHeaderWithResult :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.Result -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithResult original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "declarator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "throws")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the throws field of hydra.java.syntax.MethodHeader
methodHeaderWithThrows :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithThrows original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "result")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "declarator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.MethodInvocation
methodInvocation :: Phantoms.TTerm Syntax.MethodInvocation_Header -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MethodInvocation
methodInvocation header arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.MethodInvocation
methodInvocationArguments :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm [Syntax.Expression]
methodInvocationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.MethodInvocation
methodInvocationHeader :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.MethodInvocation_Header
methodInvocationHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
        Core.projectionField = (Core.Name "header")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.MethodInvocation
methodInvocationWithArguments :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MethodInvocation
methodInvocationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.MethodInvocation
methodInvocationWithHeader :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.MethodInvocation_Header -> Phantoms.TTerm Syntax.MethodInvocation
methodInvocationWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodInvocation_Complex
methodInvocation_Complex :: Phantoms.TTerm Syntax.MethodInvocation_Variant -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_Complex variant typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexIdentifier :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.Identifier
methodInvocation_ComplexIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexTypeArguments :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm [Syntax.TypeArgument]
methodInvocation_ComplexTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variant field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexVariant :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_ComplexVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
        Core.projectionField = (Core.Name "variant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexWithIdentifier :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_ComplexWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexWithTypeArguments :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_ComplexWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.java.syntax.MethodInvocation_Complex
methodInvocation_ComplexWithVariant :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.MethodInvocation_Variant -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_ComplexWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the complex variant of hydra.java.syntax.MethodInvocation_Header
methodInvocation_HeaderComplex :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.MethodInvocation_Header
methodInvocation_HeaderComplex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.MethodInvocation_Header
methodInvocation_HeaderSimple :: Phantoms.TTerm Syntax.MethodName -> Phantoms.TTerm Syntax.MethodInvocation_Header
methodInvocation_HeaderSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantExpression :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantSuper :: Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantSuper =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantType :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeSuper variant of hydra.java.syntax.MethodInvocation_Variant
methodInvocation_VariantTypeSuper :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantTypeSuper x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSuper"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the abstract variant of hydra.java.syntax.MethodModifier
methodModifierAbstract :: Phantoms.TTerm Syntax.MethodModifier
methodModifierAbstract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.MethodModifier
methodModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.MethodModifier
methodModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.MethodModifier
methodModifierFinal :: Phantoms.TTerm Syntax.MethodModifier
methodModifierFinal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the native variant of hydra.java.syntax.MethodModifier
methodModifierNative :: Phantoms.TTerm Syntax.MethodModifier
methodModifierNative =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "native"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.java.syntax.MethodModifier
methodModifierPrivate :: Phantoms.TTerm Syntax.MethodModifier
methodModifierPrivate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the protected variant of hydra.java.syntax.MethodModifier
methodModifierProtected :: Phantoms.TTerm Syntax.MethodModifier
methodModifierProtected =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the public variant of hydra.java.syntax.MethodModifier
methodModifierPublic :: Phantoms.TTerm Syntax.MethodModifier
methodModifierPublic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the static variant of hydra.java.syntax.MethodModifier
methodModifierStatic :: Phantoms.TTerm Syntax.MethodModifier
methodModifierStatic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictfp variant of hydra.java.syntax.MethodModifier
methodModifierStrictfp :: Phantoms.TTerm Syntax.MethodModifier
methodModifierStrictfp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the synchronized variant of hydra.java.syntax.MethodModifier
methodModifierSynchronized :: Phantoms.TTerm Syntax.MethodModifier
methodModifierSynchronized =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.MethodName wrapper
methodName :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodName
methodName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MethodName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the array variant of hydra.java.syntax.MethodReference
methodReferenceArray :: Phantoms.TTerm Syntax.MethodReference_Array -> Phantoms.TTerm Syntax.MethodReference
methodReferenceArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.MethodReference
methodReferenceExpression :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.MethodReference
methodReferenceExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the new variant of hydra.java.syntax.MethodReference
methodReferenceNew :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm Syntax.MethodReference
methodReferenceNew x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.MethodReference
methodReferencePrimary :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.MethodReference
methodReferencePrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the referenceType variant of hydra.java.syntax.MethodReference
methodReferenceReferenceType :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.MethodReference
methodReferenceReferenceType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "referenceType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.MethodReference
methodReferenceSuper :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Syntax.MethodReference
methodReferenceSuper x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.MethodReference_Array wrapper
methodReference_Array :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.MethodReference_Array
methodReference_Array x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.MethodReference_Array"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.MethodReference_Expression
methodReference_Expression :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_Expression name typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionIdentifier :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.Identifier
methodReference_ExpressionIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionName :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.ExpressionName
methodReference_ExpressionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_ExpressionTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_ExpressionWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionWithName :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_ExpressionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_Expression
methodReference_ExpressionWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_ExpressionWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_New
methodReference_New :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_New
methodReference_New classType typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Phantoms.unTTerm classType)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))
-- | DSL accessor for the classType field of hydra.java.syntax.MethodReference_New
methodReference_NewClassType :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm Syntax.ClassType
methodReference_NewClassType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
        Core.projectionField = (Core.Name "classType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_New
methodReference_NewTypeArguments :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_NewTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the classType field of hydra.java.syntax.MethodReference_New
methodReference_NewWithClassType :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.MethodReference_New
methodReference_NewWithClassType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_New
methodReference_NewWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_New
methodReference_NewWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_New"),
              Core.projectionField = (Core.Name "classType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_Primary
methodReference_Primary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_Primary primary typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryIdentifier :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Identifier
methodReference_PrimaryIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primary field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryPrimary :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Primary
methodReference_PrimaryPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionField = (Core.Name "primary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_PrimaryTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_PrimaryWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryWithPrimary :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_PrimaryWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_Primary
methodReference_PrimaryWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_PrimaryWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceType :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceType referenceType typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Phantoms.unTTerm referenceType)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeIdentifier :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.Identifier
methodReference_ReferenceTypeIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the referenceType field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeReferenceType :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.ReferenceType
methodReference_ReferenceTypeReferenceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionField = (Core.Name "referenceType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeTypeArguments :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_ReferenceTypeTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "referenceType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the referenceType field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithReferenceType :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithReferenceType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "referenceType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.MethodReference_Super
methodReference_Super :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_Super typeArguments identifier super =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Phantoms.unTTerm super)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.MethodReference_Super
methodReference_SuperIdentifier :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Syntax.Identifier
methodReference_SuperIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the super field of hydra.java.syntax.MethodReference_Super
methodReference_SuperSuper :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Bool
methodReference_SuperSuper x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionField = (Core.Name "super")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.MethodReference_Super
methodReference_SuperTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_SuperTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.MethodReference_Super
methodReference_SuperWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_SuperWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "super")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the super field of hydra.java.syntax.MethodReference_Super
methodReference_SuperWithSuper :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_SuperWithSuper original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.MethodReference_Super
methodReference_SuperWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_SuperWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "super")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ModularCompilationUnit
modularCompilationUnit :: Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.ModularCompilationUnit
modularCompilationUnit imports module_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)}]}))
-- | DSL accessor for the imports field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitImports :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration]
modularCompilationUnitImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
        Core.projectionField = (Core.Name "imports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the module field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitModule :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm Syntax.ModuleDeclaration
modularCompilationUnitModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
        Core.projectionField = (Core.Name "module")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the imports field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitWithImports :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.ModularCompilationUnit
modularCompilationUnitWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
              Core.projectionField = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the module field of hydra.java.syntax.ModularCompilationUnit
modularCompilationUnitWithModule :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.ModularCompilationUnit
modularCompilationUnitWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModularCompilationUnit"),
              Core.projectionField = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ModuleDeclaration
moduleDeclaration :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm [Syntax.ModuleDirective] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclaration annotations open identifiers directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Phantoms.unTTerm open)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm identifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Phantoms.unTTerm directives)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationAnnotations :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Annotation]
moduleDeclarationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the directives field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationDirectives :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.ModuleDirective]
moduleDeclarationDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "directives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifiers field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationIdentifiers :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Identifier]
moduleDeclarationIdentifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "identifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the open field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationOpen :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Bool
moduleDeclarationOpen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "open")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithAnnotations :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "open")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the directives field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithDirectives :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.ModuleDirective] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "open")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifiers field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithIdentifiers :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithIdentifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "open")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the open field of hydra.java.syntax.ModuleDeclaration
moduleDeclarationWithOpen :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithOpen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "directives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the exports variant of hydra.java.syntax.ModuleDirective
moduleDirectiveExports :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveExports x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exports"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the opens variant of hydra.java.syntax.ModuleDirective
moduleDirectiveOpens :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveOpens x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the provides variant of hydra.java.syntax.ModuleDirective
moduleDirectiveProvides :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveProvides x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "provides"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the requires variant of hydra.java.syntax.ModuleDirective
moduleDirectiveRequires :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveRequires x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "requires"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the uses variant of hydra.java.syntax.ModuleDirective
moduleDirectiveUses :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveUses x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uses"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpens :: Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm [Syntax.ModuleName] -> Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpens package modules =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm modules)}]}))
-- | DSL accessor for the modules field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensModules :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm [Syntax.ModuleName]
moduleDirective_ExportsOrOpensModules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionField = (Core.Name "modules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the package field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensPackage :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.PackageName
moduleDirective_ExportsOrOpensPackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionField = (Core.Name "package")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the modules field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithModules :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm [Syntax.ModuleName] -> Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithModules original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
              Core.projectionField = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the package field of hydra.java.syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithPackage :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_ExportsOrOpens"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ModuleDirective_Provides
moduleDirective_Provides :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.ModuleDirective_Provides
moduleDirective_Provides to with =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm to)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm with)}]}))
-- | DSL accessor for the to field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesTo :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm Syntax.TypeName
moduleDirective_ProvidesTo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
        Core.projectionField = (Core.Name "to")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the with field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWith :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm [Syntax.TypeName]
moduleDirective_ProvidesWith x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
        Core.projectionField = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the to field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithTo :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithTo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the with field of hydra.java.syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithWith :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithWith original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Provides"),
              Core.projectionField = (Core.Name "to")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ModuleDirective_Requires
moduleDirective_Requires :: Phantoms.TTerm [Syntax.RequiresModifier] -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.ModuleDirective_Requires
moduleDirective_Requires modifiers module_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)}]}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresModifiers :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm [Syntax.RequiresModifier]
moduleDirective_RequiresModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the module field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresModule :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm Syntax.ModuleName
moduleDirective_RequiresModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
        Core.projectionField = (Core.Name "module")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the modifiers field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModifiers :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm [Syntax.RequiresModifier] -> Phantoms.TTerm Syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
              Core.projectionField = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the module field of hydra.java.syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModule :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleDirective_Requires"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.ModuleName
moduleName :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm Syntax.ModuleName
moduleName identifier name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ModuleName
moduleNameIdentifier :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Identifier
moduleNameIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.java.syntax.ModuleName
moduleNameName :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm (Maybe Syntax.ModuleName)
moduleNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.ModuleName
moduleNameWithIdentifier :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ModuleName
moduleNameWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.java.syntax.ModuleName
moduleNameWithName :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm Syntax.ModuleName
moduleNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ModuleName"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the divide variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionDivide :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionDivide x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the mod variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionMod :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionMod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the times variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionTimes :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionTimes x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.MultiplicativeExpression
multiplicativeExpressionUnary :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionUnary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_Binary :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression_Binary
multiplicativeExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryLhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryRhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.UnaryExpression
multiplicativeExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.MultiplicativeExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.NormalAnnotation
normalAnnotation :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.ElementValuePair] -> Phantoms.TTerm Syntax.NormalAnnotation
normalAnnotation typeName pairs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Phantoms.unTTerm pairs)}]}))
-- | DSL accessor for the pairs field of hydra.java.syntax.NormalAnnotation
normalAnnotationPairs :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm [Syntax.ElementValuePair]
normalAnnotationPairs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
        Core.projectionField = (Core.Name "pairs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.java.syntax.NormalAnnotation
normalAnnotationTypeName :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm Syntax.TypeName
normalAnnotationTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the pairs field of hydra.java.syntax.NormalAnnotation
normalAnnotationWithPairs :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm [Syntax.ElementValuePair] -> Phantoms.TTerm Syntax.NormalAnnotation
normalAnnotationWithPairs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.java.syntax.NormalAnnotation
normalAnnotationWithTypeName :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.NormalAnnotation
normalAnnotationWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalAnnotation"),
              Core.projectionField = (Core.Name "pairs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.NormalClassDeclaration
normalClassDeclaration :: Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm (Maybe Syntax.ClassType) -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclaration modifiers identifier parameters extends implements permits body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Phantoms.unTTerm extends)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm implements)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Phantoms.unTTerm permits)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationBody :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.ClassBody
normalClassDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the extends field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationExtends :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm (Maybe Syntax.ClassType)
normalClassDeclarationExtends x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "extends")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationIdentifier :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
normalClassDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationImplements :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.InterfaceType]
normalClassDeclarationImplements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "implements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationModifiers :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.ClassModifier]
normalClassDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationParameters :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.TypeParameter]
normalClassDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the permits field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationPermits :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.TypeName]
normalClassDeclarationPermits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "permits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithBody :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the extends field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithExtends :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm (Maybe Syntax.ClassType) -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithExtends original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithIdentifier :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the implements field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithImplements :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithImplements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithModifiers :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithParameters :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the permits field of hydra.java.syntax.NormalClassDeclaration
normalClassDeclarationWithPermits :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithPermits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclaration :: Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.InterfaceBody -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclaration modifiers identifier parameters extends permits body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Phantoms.unTTerm extends)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Phantoms.unTTerm permits)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationBody :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceBody
normalInterfaceDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the extends field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationExtends :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceType]
normalInterfaceDeclarationExtends x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "extends")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationIdentifier :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
normalInterfaceDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationModifiers :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier]
normalInterfaceDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationParameters :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.TypeParameter]
normalInterfaceDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the permits field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationPermits :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.TypeName]
normalInterfaceDeclarationPermits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "permits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithBody :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceBody -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the extends field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithExtends :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithExtends original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithIdentifier :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithModifiers :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithParameters :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "permits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the permits field of hydra.java.syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithPermits :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithPermits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "permits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the array variant of hydra.java.syntax.NumericTypeArray
numericTypeArrayArray :: Phantoms.TTerm Syntax.NumericTypeArray -> Phantoms.TTerm Syntax.NumericTypeArray
numericTypeArrayArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.NumericTypeArray
numericTypeArraySimple :: Phantoms.TTerm Syntax.NumericType -> Phantoms.TTerm Syntax.NumericTypeArray
numericTypeArraySimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the floatingPoint variant of hydra.java.syntax.NumericType
numericTypeFloatingPoint :: Phantoms.TTerm Syntax.FloatingPointType -> Phantoms.TTerm Syntax.NumericType
numericTypeFloatingPoint x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integral variant of hydra.java.syntax.NumericType
numericTypeIntegral :: Phantoms.TTerm Syntax.IntegralType -> Phantoms.TTerm Syntax.NumericType
numericTypeIntegral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnit :: Phantoms.TTerm (Maybe Syntax.PackageDeclaration) -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm [Syntax.TopLevelClassOrInterfaceDeclarationWithComments] -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnit package imports types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))
-- | DSL accessor for the imports field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitImports :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration]
ordinaryCompilationUnitImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionField = (Core.Name "imports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the package field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitPackage :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm (Maybe Syntax.PackageDeclaration)
ordinaryCompilationUnitPackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionField = (Core.Name "package")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the types field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitTypes :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.TopLevelClassOrInterfaceDeclarationWithComments]
ordinaryCompilationUnitTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionField = (Core.Name "types")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the imports field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithImports :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the package field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithPackage :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm (Maybe Syntax.PackageDeclaration) -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the types field of hydra.java.syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithTypes :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.TopLevelClassOrInterfaceDeclarationWithComments] -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.PackageDeclaration
packageDeclaration :: Phantoms.TTerm [Syntax.PackageModifier] -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageDeclaration
packageDeclaration modifiers identifiers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm identifiers)}]}))
-- | DSL accessor for the identifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationIdentifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.Identifier]
packageDeclarationIdentifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
        Core.projectionField = (Core.Name "identifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationModifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.PackageModifier]
packageDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationWithIdentifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageDeclaration
packageDeclarationWithIdentifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.PackageDeclaration
packageDeclarationWithModifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.PackageModifier] -> Phantoms.TTerm Syntax.PackageDeclaration
packageDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PackageDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.PackageModifier wrapper
packageModifier :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.PackageModifier
packageModifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageModifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PackageName wrapper
packageName :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageName
packageName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PackageOrTypeName wrapper
packageOrTypeName :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageOrTypeName
packageOrTypeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PackageOrTypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the record variant of hydra.java.syntax.Pattern
patternRecord :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.Pattern
patternRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.java.syntax.Pattern
patternType :: Phantoms.TTerm Syntax.TypePattern -> Phantoms.TTerm Syntax.Pattern
patternType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.PostDecrementExpression wrapper
postDecrementExpression :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.PostDecrementExpression
postDecrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PostDecrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PostIncrementExpression wrapper
postIncrementExpression :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.PostIncrementExpression
postIncrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PostIncrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the name variant of hydra.java.syntax.PostfixExpression
postfixExpressionName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the postDecrement variant of hydra.java.syntax.PostfixExpression
postfixExpressionPostDecrement :: Phantoms.TTerm Syntax.PostDecrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPostDecrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the postIncrement variant of hydra.java.syntax.PostfixExpression
postfixExpressionPostIncrement :: Phantoms.TTerm Syntax.PostIncrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPostIncrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primary variant of hydra.java.syntax.PostfixExpression
postfixExpressionPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.PreDecrementExpression wrapper
preDecrementExpression :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.PreDecrementExpression
preDecrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PreDecrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.PreIncrementExpression wrapper
preIncrementExpression :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.PreIncrementExpression
preIncrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.PreIncrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the arrayCreation variant of hydra.java.syntax.Primary
primaryArrayCreation :: Phantoms.TTerm Syntax.ArrayCreationExpression -> Phantoms.TTerm Syntax.Primary
primaryArrayCreation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayCreation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the noNewArray variant of hydra.java.syntax.Primary
primaryNoNewArray :: Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression -> Phantoms.TTerm Syntax.Primary
primaryNoNewArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noNewArray"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the arrayAccess variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionArrayAccess :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionArrayAccess x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the classInstance variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassInstance :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassInstance x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstance"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the classLiteral variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassLiteral :: Phantoms.TTerm Syntax.ClassLiteral -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionClassLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classLiteral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the dotThis variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionDotThis :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionDotThis x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dotThis"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionFieldAccess x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the methodInvocation variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodInvocation :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodInvocation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the methodReference variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodReference :: Phantoms.TTerm Syntax.MethodReference -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionMethodReference x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodReference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the parens variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionParens :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionParens x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the this variant of hydra.java.syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionThis :: Phantoms.TTerm Syntax.PrimaryNoNewArrayExpression
primaryNoNewArrayExpressionThis =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimaryNoNewArrayExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the boolean variant of hydra.java.syntax.PrimitiveType
primitiveTypeBoolean :: Phantoms.TTerm Syntax.PrimitiveType
primitiveTypeBoolean =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the numeric variant of hydra.java.syntax.PrimitiveType
primitiveTypeNumeric :: Phantoms.TTerm Syntax.NumericType -> Phantoms.TTerm Syntax.PrimitiveType
primitiveTypeNumeric x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotations :: Phantoms.TTerm Syntax.PrimitiveType -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotations type_ annotations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsAnnotations :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.Annotation]
primitiveTypeWithAnnotationsAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsType :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.PrimitiveType
primitiveTypeWithAnnotationsType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithAnnotations :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithType :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.PrimitiveType -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.PrimitiveTypeWithAnnotations"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.ReceiverParameter
receiverParameter :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameter annotations unannType identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Phantoms.unTTerm unannType)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.ReceiverParameter
receiverParameterAnnotations :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm [Syntax.Annotation]
receiverParameterAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.ReceiverParameter
receiverParameterIdentifier :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm (Maybe Syntax.Identifier)
receiverParameterIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the unannType field of hydra.java.syntax.ReceiverParameter
receiverParameterUnannType :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm Syntax.UnannType
receiverParameterUnannType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
        Core.projectionField = (Core.Name "unannType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.ReceiverParameter
receiverParameterWithAnnotations :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameterWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "unannType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.ReceiverParameter
receiverParameterWithIdentifier :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameterWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "unannType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the unannType field of hydra.java.syntax.ReceiverParameter
receiverParameterWithUnannType :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameterWithUnannType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.RecordBody wrapper
recordBody :: Phantoms.TTerm [Syntax.RecordBodyDeclaration] -> Phantoms.TTerm Syntax.RecordBody
recordBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the classBody variant of hydra.java.syntax.RecordBodyDeclaration
recordBodyDeclarationClassBody :: Phantoms.TTerm Syntax.ClassBodyDeclaration -> Phantoms.TTerm Syntax.RecordBodyDeclaration
recordBodyDeclarationClassBody x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classBody"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the compactConstructor variant of hydra.java.syntax.RecordBodyDeclaration
recordBodyDeclarationCompactConstructor :: Phantoms.TTerm Syntax.CompactConstructorDeclaration -> Phantoms.TTerm Syntax.RecordBodyDeclaration
recordBodyDeclarationCompactConstructor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compactConstructor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.RecordComponentModifier wrapper
recordComponentModifier :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.RecordComponentModifier
recordComponentModifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordComponentModifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the simple variant of hydra.java.syntax.RecordComponent
recordComponentSimple :: Phantoms.TTerm Syntax.RecordComponent_Simple -> Phantoms.TTerm Syntax.RecordComponent
recordComponentSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variableArity variant of hydra.java.syntax.RecordComponent
recordComponentVariableArity :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm Syntax.RecordComponent
recordComponentVariableArity x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.RecordComponent_Simple
recordComponent_Simple :: Phantoms.TTerm [Syntax.RecordComponentModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.RecordComponent_Simple
recordComponent_Simple modifiers type_ identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleIdentifier :: Phantoms.TTerm Syntax.RecordComponent_Simple -> Phantoms.TTerm Syntax.Identifier
recordComponent_SimpleIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleModifiers :: Phantoms.TTerm Syntax.RecordComponent_Simple -> Phantoms.TTerm [Syntax.RecordComponentModifier]
recordComponent_SimpleModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleType :: Phantoms.TTerm Syntax.RecordComponent_Simple -> Phantoms.TTerm Syntax.UnannType
recordComponent_SimpleType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleWithIdentifier :: Phantoms.TTerm Syntax.RecordComponent_Simple -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.RecordComponent_Simple
recordComponent_SimpleWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleWithModifiers :: Phantoms.TTerm Syntax.RecordComponent_Simple -> Phantoms.TTerm [Syntax.RecordComponentModifier] -> Phantoms.TTerm Syntax.RecordComponent_Simple
recordComponent_SimpleWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.RecordComponent_Simple
recordComponent_SimpleWithType :: Phantoms.TTerm Syntax.RecordComponent_Simple -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.RecordComponent_Simple
recordComponent_SimpleWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordComponent_Simple"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.RecordDeclaration
recordDeclaration :: Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.RecordHeader -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm Syntax.RecordDeclaration
recordDeclaration modifiers identifier parameters header implements body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm implements)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.RecordDeclaration
recordDeclarationBody :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm Syntax.RecordBody
recordDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the header field of hydra.java.syntax.RecordDeclaration
recordDeclarationHeader :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm Syntax.RecordHeader
recordDeclarationHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionField = (Core.Name "header")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.RecordDeclaration
recordDeclarationIdentifier :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
recordDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the implements field of hydra.java.syntax.RecordDeclaration
recordDeclarationImplements :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm [Syntax.InterfaceType]
recordDeclarationImplements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionField = (Core.Name "implements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.RecordDeclaration
recordDeclarationModifiers :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm [Syntax.ClassModifier]
recordDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameters field of hydra.java.syntax.RecordDeclaration
recordDeclarationParameters :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm [Syntax.TypeParameter]
recordDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithBody :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm Syntax.RecordDeclaration
recordDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the header field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithHeader :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm Syntax.RecordHeader -> Phantoms.TTerm Syntax.RecordDeclaration
recordDeclarationWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithIdentifier :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.RecordDeclaration
recordDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the implements field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithImplements :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.RecordDeclaration
recordDeclarationWithImplements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithModifiers :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.RecordDeclaration
recordDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the parameters field of hydra.java.syntax.RecordDeclaration
recordDeclarationWithParameters :: Phantoms.TTerm Syntax.RecordDeclaration -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.RecordDeclaration
recordDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "implements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordDeclaration"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.RecordHeader wrapper
recordHeader :: Phantoms.TTerm [Syntax.RecordComponent] -> Phantoms.TTerm Syntax.RecordHeader
recordHeader x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.RecordHeader"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.RecordPattern
recordPattern :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.RecordPattern
recordPattern type_ patterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)}]}))
-- | DSL accessor for the patterns field of hydra.java.syntax.RecordPattern
recordPatternPatterns :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm [Syntax.Pattern]
recordPatternPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
        Core.projectionField = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.RecordPattern
recordPatternType :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.ReferenceType
recordPatternType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the patterns field of hydra.java.syntax.RecordPattern
recordPatternWithPatterns :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.RecordPattern
recordPatternWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.RecordPattern
recordPatternWithType :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.RecordPattern
recordPatternWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RecordPattern"),
              Core.projectionField = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the array variant of hydra.java.syntax.ReferenceType
referenceTypeArray :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.ReferenceType
referenceTypeClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeClassOrInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.ReferenceType
referenceTypeVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the greaterThan variant of hydra.java.syntax.RelationalExpression
relationalExpressionGreaterThan :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionGreaterThan x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the greaterThanEqual variant of hydra.java.syntax.RelationalExpression
relationalExpressionGreaterThanEqual :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionGreaterThanEqual x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the instanceofExpression variant of hydra.java.syntax.RelationalExpression
relationalExpressionInstanceofExpression :: Phantoms.TTerm Syntax.InstanceofExpression -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionInstanceofExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceofExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lessThan variant of hydra.java.syntax.RelationalExpression
relationalExpressionLessThan :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionLessThan x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lessThanEqual variant of hydra.java.syntax.RelationalExpression
relationalExpressionLessThanEqual :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionLessThanEqual x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.RelationalExpression
relationalExpressionSimple :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThan :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThan lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqual :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqual lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_GreaterThanEqualLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_GreaterThanEqualRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThanEqual"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_GreaterThanLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_GreaterThanRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_GreaterThan"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThan :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThan
relationalExpression_LessThan lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL constructor for hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqual :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqual lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_LessThanEqualLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_LessThanEqualRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThanEqual"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_LessThanLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_LessThanRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.RelationalExpression_LessThan"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the static variant of hydra.java.syntax.RequiresModifier
requiresModifierStatic :: Phantoms.TTerm Syntax.RequiresModifier
requiresModifierStatic =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transitive variant of hydra.java.syntax.RequiresModifier
requiresModifierTransitive :: Phantoms.TTerm Syntax.RequiresModifier
requiresModifierTransitive =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transitive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the local variant of hydra.java.syntax.Resource
resourceLocal :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Resource
resourceLocal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.ResourceSpecification wrapper
resourceSpecification :: Phantoms.TTerm [Syntax.Resource] -> Phantoms.TTerm Syntax.ResourceSpecification
resourceSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ResourceSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the variable variant of hydra.java.syntax.Resource
resourceVariable :: Phantoms.TTerm Syntax.VariableAccess -> Phantoms.TTerm Syntax.Resource
resourceVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.Resource_Local
resource_Local :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Resource_Local
resource_Local modifiers type_ identifier expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.java.syntax.Resource_Local
resource_LocalExpression :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Expression
resource_LocalExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.Resource_Local
resource_LocalIdentifier :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Identifier
resource_LocalIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.Resource_Local
resource_LocalModifiers :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm [Syntax.VariableModifier]
resource_LocalModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.Resource_Local
resource_LocalType :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.LocalVariableType
resource_LocalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.java.syntax.Resource_Local
resource_LocalWithExpression :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.Resource_Local
resource_LocalWithIdentifier :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.Resource_Local
resource_LocalWithModifiers :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.Resource_Local
resource_LocalWithType :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the type variant of hydra.java.syntax.Result
resultType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.Result
resultType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the void variant of hydra.java.syntax.Result
resultVoid :: Phantoms.TTerm Syntax.Result
resultVoid =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.java.syntax.ReturnStatement wrapper
returnStatement :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ReturnStatement
returnStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ReturnStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the shiftLeft variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftLeft :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionShiftLeft x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the shiftRight variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftRight :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionShiftRight x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the shiftRightZeroFill variant of hydra.java.syntax.ShiftExpression
shiftExpressionShiftRightZeroFill :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionShiftRightZeroFill x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unary variant of hydra.java.syntax.ShiftExpression
shiftExpressionUnary :: Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionUnary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.ShiftExpression_Binary
shiftExpression_Binary :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.ShiftExpression_Binary
shiftExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryLhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryRhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
shiftExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.ShiftExpression_Binary
shiftExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.java.syntax.ShiftExpression_Binary
shiftExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.ShiftExpression_Binary
shiftExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.ShiftExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.SimpleTypeName wrapper
simpleTypeName :: Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.SimpleTypeName
simpleTypeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.SimpleTypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.SingleElementAnnotation
singleElementAnnotation :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.ElementValue) -> Phantoms.TTerm Syntax.SingleElementAnnotation
singleElementAnnotation name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the name field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationName :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm Syntax.TypeName
singleElementAnnotationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationValue :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm (Maybe Syntax.ElementValue)
singleElementAnnotationValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationWithName :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.SingleElementAnnotation
singleElementAnnotationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.java.syntax.SingleElementAnnotation
singleElementAnnotationWithValue :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm (Maybe Syntax.ElementValue) -> Phantoms.TTerm Syntax.SingleElementAnnotation
singleElementAnnotationWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleElementAnnotation"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclaration :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SingleStaticImportDeclaration
singleStaticImportDeclaration typeName identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the identifier field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationIdentifier :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.Identifier
singleStaticImportDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationTypeName :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.TypeName
singleStaticImportDeclarationTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithIdentifier :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.java.syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithTypeName :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SingleStaticImportDeclaration"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.SingleTypeImportDeclaration wrapper
singleTypeImportDeclaration :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.SingleTypeImportDeclaration
singleTypeImportDeclaration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.SingleTypeImportDeclaration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the assignment variant of hydra.java.syntax.StatementExpression
statementExpressionAssignment :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the classInstanceCreation variant of hydra.java.syntax.StatementExpression
statementExpressionClassInstanceCreation :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionClassInstanceCreation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstanceCreation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the methodInvocation variant of hydra.java.syntax.StatementExpression
statementExpressionMethodInvocation :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionMethodInvocation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the postDecrement variant of hydra.java.syntax.StatementExpression
statementExpressionPostDecrement :: Phantoms.TTerm Syntax.PostDecrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPostDecrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the postIncrement variant of hydra.java.syntax.StatementExpression
statementExpressionPostIncrement :: Phantoms.TTerm Syntax.PostIncrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPostIncrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the preDecrement variant of hydra.java.syntax.StatementExpression
statementExpressionPreDecrement :: Phantoms.TTerm Syntax.PreDecrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPreDecrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the preIncrement variant of hydra.java.syntax.StatementExpression
statementExpressionPreIncrement :: Phantoms.TTerm Syntax.PreIncrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPreIncrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the for variant of hydra.java.syntax.Statement
statementFor :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Statement
statementFor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ifThen variant of hydra.java.syntax.Statement
statementIfThen :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Statement
statementIfThen x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThen"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ifThenElse variant of hydra.java.syntax.Statement
statementIfThenElse :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.Statement
statementIfThenElse x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the labeled variant of hydra.java.syntax.Statement
statementLabeled :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
statementLabeled x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the for variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfFor :: Phantoms.TTerm Syntax.ForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfFor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ifThenElse variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfIfThenElse :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfIfThenElse x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the labeled variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfLabeled :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfLabeled x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the while variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfWhile :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfWhile x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the withoutTrailing variant of hydra.java.syntax.StatementNoShortIf
statementNoShortIfWithoutTrailing :: Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfWithoutTrailing x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the while variant of hydra.java.syntax.Statement
statementWhile :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement
statementWhile x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the withoutTrailing variant of hydra.java.syntax.Statement
statementWithoutTrailing :: Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement -> Phantoms.TTerm Syntax.Statement
statementWithoutTrailing x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the assert variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementAssert :: Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementAssert x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assert"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the block variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the break variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBreak :: Phantoms.TTerm Syntax.BreakStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBreak x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the continue variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementContinue :: Phantoms.TTerm Syntax.ContinueStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementContinue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the do variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementDo :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementDo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the empty variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementEmpty :: Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementEmpty =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the expression variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementExpression :: Phantoms.TTerm Syntax.ExpressionStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the return variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementReturn :: Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementReturn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the switch variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSwitch :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSwitch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the synchronized variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSynchronized :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSynchronized x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the throw variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementThrow :: Phantoms.TTerm Syntax.ThrowStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementThrow x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the try variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementTry :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementTry x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the yield variant of hydra.java.syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementYield :: Phantoms.TTerm Syntax.YieldStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementYield x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.java.syntax.StaticImportOnDemandDeclaration wrapper
staticImportOnDemandDeclaration :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.StaticImportOnDemandDeclaration
staticImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StaticImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.StaticInitializer wrapper
staticInitializer :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.StaticInitializer
staticInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StaticInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.StringLiteral wrapper
stringLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteral
stringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.StringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the legacy variant of hydra.java.syntax.SwitchBlock
switchBlockLegacy :: Phantoms.TTerm Syntax.SwitchBlock_Legacy -> Phantoms.TTerm Syntax.SwitchBlock
switchBlockLegacy x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "legacy"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the rules variant of hydra.java.syntax.SwitchBlock
switchBlockRules :: Phantoms.TTerm [Syntax.SwitchRule] -> Phantoms.TTerm Syntax.SwitchBlock
switchBlockRules x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rules"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroup :: Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.SwitchBlockStatementGroup
switchBlockStatementGroup labels statements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm labels)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)}]}))
-- | DSL accessor for the labels field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupLabels :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.SwitchLabel]
switchBlockStatementGroupLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionField = (Core.Name "labels")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statements field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupStatements :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.BlockStatement]
switchBlockStatementGroupStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionField = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the labels field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithLabels :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm Syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
              Core.projectionField = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.java.syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithStatements :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlockStatementGroup"),
              Core.projectionField = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.SwitchBlock_Legacy
switchBlock_Legacy :: Phantoms.TTerm [Syntax.SwitchBlockStatementGroup] -> Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm Syntax.SwitchBlock_Legacy
switchBlock_Legacy groups trailingLabels =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Phantoms.unTTerm groups)},
        Core.Field {
          Core.fieldName = (Core.Name "trailingLabels"),
          Core.fieldTerm = (Phantoms.unTTerm trailingLabels)}]}))
-- | DSL accessor for the groups field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyGroups :: Phantoms.TTerm Syntax.SwitchBlock_Legacy -> Phantoms.TTerm [Syntax.SwitchBlockStatementGroup]
switchBlock_LegacyGroups x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
        Core.projectionField = (Core.Name "groups")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the trailingLabels field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyTrailingLabels :: Phantoms.TTerm Syntax.SwitchBlock_Legacy -> Phantoms.TTerm [Syntax.SwitchLabel]
switchBlock_LegacyTrailingLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
        Core.projectionField = (Core.Name "trailingLabels")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the groups field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyWithGroups :: Phantoms.TTerm Syntax.SwitchBlock_Legacy -> Phantoms.TTerm [Syntax.SwitchBlockStatementGroup] -> Phantoms.TTerm Syntax.SwitchBlock_Legacy
switchBlock_LegacyWithGroups original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trailingLabels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
              Core.projectionField = (Core.Name "trailingLabels")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the trailingLabels field of hydra.java.syntax.SwitchBlock_Legacy
switchBlock_LegacyWithTrailingLabels :: Phantoms.TTerm Syntax.SwitchBlock_Legacy -> Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm Syntax.SwitchBlock_Legacy
switchBlock_LegacyWithTrailingLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchBlock_Legacy"),
              Core.projectionField = (Core.Name "groups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trailingLabels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.SwitchExpression
switchExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchBlock -> Phantoms.TTerm Syntax.SwitchExpression
switchExpression cond block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.SwitchExpression
switchExpressionBlock :: Phantoms.TTerm Syntax.SwitchExpression -> Phantoms.TTerm Syntax.SwitchBlock
switchExpressionBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.SwitchExpression
switchExpressionCond :: Phantoms.TTerm Syntax.SwitchExpression -> Phantoms.TTerm Syntax.Expression
switchExpressionCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SwitchExpression
switchExpressionWithBlock :: Phantoms.TTerm Syntax.SwitchExpression -> Phantoms.TTerm Syntax.SwitchBlock -> Phantoms.TTerm Syntax.SwitchExpression
switchExpressionWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.SwitchExpression
switchExpressionWithCond :: Phantoms.TTerm Syntax.SwitchExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchExpression
switchExpressionWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchExpression"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the case variant of hydra.java.syntax.SwitchLabel
switchLabelCase :: Phantoms.TTerm [Syntax.CaseConstant] -> Phantoms.TTerm Syntax.SwitchLabel
switchLabelCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the casePattern variant of hydra.java.syntax.SwitchLabel
switchLabelCasePattern :: Phantoms.TTerm Syntax.CasePattern -> Phantoms.TTerm Syntax.SwitchLabel
switchLabelCasePattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "casePattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the default variant of hydra.java.syntax.SwitchLabel
switchLabelDefault :: Phantoms.TTerm Syntax.SwitchLabel
switchLabelDefault =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the null variant of hydra.java.syntax.SwitchLabel
switchLabelNull :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.SwitchLabel
switchLabelNull x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchRule
switchRule :: Phantoms.TTerm Syntax.SwitchLabel -> Phantoms.TTerm Syntax.SwitchRule_Body -> Phantoms.TTerm Syntax.SwitchRule
switchRule label body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.SwitchRule
switchRuleBody :: Phantoms.TTerm Syntax.SwitchRule -> Phantoms.TTerm Syntax.SwitchRule_Body
switchRuleBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.java.syntax.SwitchRule
switchRuleLabel :: Phantoms.TTerm Syntax.SwitchRule -> Phantoms.TTerm Syntax.SwitchLabel
switchRuleLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.SwitchRule
switchRuleWithBody :: Phantoms.TTerm Syntax.SwitchRule -> Phantoms.TTerm Syntax.SwitchRule_Body -> Phantoms.TTerm Syntax.SwitchRule
switchRuleWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the label field of hydra.java.syntax.SwitchRule
switchRuleWithLabel :: Phantoms.TTerm Syntax.SwitchRule -> Phantoms.TTerm Syntax.SwitchLabel -> Phantoms.TTerm Syntax.SwitchRule
switchRuleWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the block variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.SwitchRule_Body
switchRule_BodyBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchRule_Body
switchRule_BodyExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the throw variant of hydra.java.syntax.SwitchRule_Body
switchRule_BodyThrow :: Phantoms.TTerm Syntax.ThrowStatement -> Phantoms.TTerm Syntax.SwitchRule_Body
switchRule_BodyThrow x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.SwitchRule_Body"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.SwitchStatement
switchStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchBlock -> Phantoms.TTerm Syntax.SwitchStatement
switchStatement cond block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.SwitchStatement
switchStatementBlock :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.SwitchBlock
switchStatementBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.SwitchStatement
switchStatementCond :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression
switchStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SwitchStatement
switchStatementWithBlock :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.SwitchBlock -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.SwitchStatement
switchStatementWithCond :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.SynchronizedStatement
synchronizedStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.SynchronizedStatement
synchronizedStatement expression block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementBlock :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Block
synchronizedStatementBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementExpression :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Expression
synchronizedStatementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementWithBlock :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.SynchronizedStatement
synchronizedStatementWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.java.syntax.SynchronizedStatement
synchronizedStatementWithExpression :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SynchronizedStatement
synchronizedStatementWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.SynchronizedStatement"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.TextBlock wrapper
textBlock :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.TextBlock
textBlock x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TextBlock"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.ThrowStatement wrapper
throwStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ThrowStatement
throwStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.ThrowStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.Throws wrapper
throws :: Phantoms.TTerm [Syntax.ExceptionType] -> Phantoms.TTerm Syntax.Throws
throws x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.Throws"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the class variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interface variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the none variant of hydra.java.syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationNone :: Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithComments :: Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithComments value comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsComments :: Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Phantoms.TTerm (Maybe String)
topLevelClassOrInterfaceDeclarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsValue :: Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclaration
topLevelClassOrInterfaceDeclarationWithCommentsValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsWithComments :: Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the value field of hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsWithValue :: Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments -> Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclaration -> Phantoms.TTerm Syntax.TopLevelClassOrInterfaceDeclarationWithComments
topLevelClassOrInterfaceDeclarationWithCommentsWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the simple variant of hydra.java.syntax.TryStatement
tryStatementSimple :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.TryStatement
tryStatementSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the withFinally variant of hydra.java.syntax.TryStatement
tryStatementWithFinally :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.TryStatement
tryStatementWithFinally x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withFinally"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the withResources variant of hydra.java.syntax.TryStatement
tryStatementWithResources :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.TryStatement
tryStatementWithResources x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withResources"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TryStatement_Simple
tryStatement_Simple :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Catches -> Phantoms.TTerm Syntax.TryStatement_Simple
tryStatement_Simple block catches =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm catches)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleBlock :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Block
tryStatement_SimpleBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleCatches :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Catches
tryStatement_SimpleCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
        Core.projectionField = (Core.Name "catches")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleWithBlock :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryStatement_Simple
tryStatement_SimpleWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
              Core.projectionField = (Core.Name "catches")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the catches field of hydra.java.syntax.TryStatement_Simple
tryStatement_SimpleWithCatches :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Catches -> Phantoms.TTerm Syntax.TryStatement_Simple
tryStatement_SimpleWithCatches original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_Simple"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinally :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm Syntax.Finally -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinally block catches finally =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm catches)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm finally)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyBlock :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Block
tryStatement_WithFinallyBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyCatches :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm (Maybe Syntax.Catches)
tryStatement_WithFinallyCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionField = (Core.Name "catches")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the finally field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyFinally :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Finally
tryStatement_WithFinallyFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
        Core.projectionField = (Core.Name "finally")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithBlock :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "catches")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the catches field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithCatches :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithCatches original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.java.syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithFinally :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Finally -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithFinally original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "catches")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatement :: Phantoms.TTerm Syntax.ResourceSpecification -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm (Maybe Syntax.Finally) -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatement resourceSpecification block catches finally =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Phantoms.unTTerm resourceSpecification)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm catches)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm finally)}]}))
-- | DSL accessor for the block field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementBlock :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.Block
tryWithResourcesStatementBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the catches field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementCatches :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Catches)
tryWithResourcesStatementCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "catches")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the finally field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementFinally :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Finally)
tryWithResourcesStatementFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "finally")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the resourceSpecification field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementResourceSpecification :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.ResourceSpecification
tryWithResourcesStatementResourceSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "resourceSpecification")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the block field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithBlock :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "resourceSpecification")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "catches")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the catches field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithCatches :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithCatches original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "resourceSpecification")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithFinally :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Finally) -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithFinally original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "resourceSpecification")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "catches")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the resourceSpecification field of hydra.java.syntax.TryWithResourcesStatement
tryWithResourcesStatementWithResourceSpecification :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.ResourceSpecification -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithResourceSpecification original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "catches")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the reference variant of hydra.java.syntax.TypeArgument
typeArgumentReference :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.TypeArgument
typeArgumentReference x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.java.syntax.TypeArgument
typeArgumentWildcard :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm Syntax.TypeArgument
typeArgumentWildcard x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the arguments variant of hydra.java.syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondArguments :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondArguments x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arguments"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the diamond variant of hydra.java.syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondDiamond :: Phantoms.TTerm Syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondDiamond =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "diamond"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the classOrInterface variant of hydra.java.syntax.TypeBound
typeBoundClassOrInterface :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm Syntax.TypeBound
typeBoundClassOrInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.java.syntax.TypeBound
typeBoundVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.TypeBound
typeBoundVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterface type_ additional =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Phantoms.unTTerm additional)}]}))
-- | DSL accessor for the additional field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceAdditional :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm [Syntax.AdditionalBound]
typeBound_ClassOrInterfaceAdditional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionField = (Core.Name "additional")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceType :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType
typeBound_ClassOrInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the additional field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithAdditional :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithAdditional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.java.syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithType :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeBound_ClassOrInterface"),
              Core.projectionField = (Core.Name "additional")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.TypeIdentifier wrapper
typeIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.TypeIdentifier
typeIdentifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeIdentifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.TypeImportOnDemandDeclaration wrapper
typeImportOnDemandDeclaration :: Phantoms.TTerm Syntax.PackageOrTypeName -> Phantoms.TTerm Syntax.TypeImportOnDemandDeclaration
typeImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.TypeName
typeName :: Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm (Maybe Syntax.PackageOrTypeName) -> Phantoms.TTerm Syntax.TypeName
typeName identifier qualifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)}]}))
-- | DSL injection for the array variant of hydra.java.syntax.TypeNameArray
typeNameArrayArray :: Phantoms.TTerm Syntax.TypeNameArray -> Phantoms.TTerm Syntax.TypeNameArray
typeNameArrayArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.java.syntax.TypeNameArray
typeNameArraySimple :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.TypeNameArray
typeNameArraySimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeName
typeNameIdentifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.TypeIdentifier
typeNameIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifier field of hydra.java.syntax.TypeName
typeNameQualifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.PackageOrTypeName)
typeNameQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
        Core.projectionField = (Core.Name "qualifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the identifier field of hydra.java.syntax.TypeName
typeNameWithIdentifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeName
typeNameWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
              Core.projectionField = (Core.Name "qualifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the qualifier field of hydra.java.syntax.TypeName
typeNameWithQualifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.PackageOrTypeName) -> Phantoms.TTerm Syntax.TypeName
typeNameWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeName"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.syntax.TypeParameter
typeParameter :: Phantoms.TTerm [Syntax.TypeParameterModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm (Maybe Syntax.TypeBound) -> Phantoms.TTerm Syntax.TypeParameter
typeParameter modifiers identifier bound =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)}]}))
-- | DSL accessor for the bound field of hydra.java.syntax.TypeParameter
typeParameterBound :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeBound)
typeParameterBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeParameter
typeParameterIdentifier :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm Syntax.TypeIdentifier
typeParameterIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.TypeParameterModifier wrapper
typeParameterModifier :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.TypeParameterModifier
typeParameterModifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypeParameterModifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.TypeParameter
typeParameterModifiers :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm [Syntax.TypeParameterModifier]
typeParameterModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bound field of hydra.java.syntax.TypeParameter
typeParameterWithBound :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeBound) -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.TypeParameter
typeParameterWithIdentifier :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.TypeParameter
typeParameterWithModifiers :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm [Syntax.TypeParameterModifier] -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.java.syntax.TypePattern wrapper
typePattern :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.TypePattern
typePattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.TypePattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the primitive variant of hydra.java.syntax.Type
typePrimitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.Type
typePrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the reference variant of hydra.java.syntax.Type
typeReference :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.Type
typeReference x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.TypeVariable
typeVariable :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeVariable
typeVariable annotations identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.TypeVariable
typeVariableAnnotations :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm [Syntax.Annotation]
typeVariableAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.TypeVariable
typeVariableIdentifier :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.TypeIdentifier
typeVariableIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.TypeVariable
typeVariableWithAnnotations :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.TypeVariable
typeVariableWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.TypeVariable
typeVariableWithIdentifier :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeVariable
typeVariableWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.TypeVariable"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the body of hydra.java.syntax.AdditionalBound
unAdditionalBound :: Phantoms.TTerm Syntax.AdditionalBound -> Phantoms.TTerm Syntax.InterfaceType
unAdditionalBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AdditionalBound")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AmbiguousName
unAmbiguousName :: Phantoms.TTerm Syntax.AmbiguousName -> Phantoms.TTerm [Syntax.Identifier]
unAmbiguousName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AmbiguousName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AndExpression
unAndExpression :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm [Syntax.EqualityExpression]
unAndExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AndExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.AnnotationInterfaceBody
unAnnotationInterfaceBody :: Phantoms.TTerm Syntax.AnnotationInterfaceBody -> Phantoms.TTerm [Syntax.AnnotationInterfaceMemberDeclaration]
unAnnotationInterfaceBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.AnnotationInterfaceBody")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ArrayInitializer
unArrayInitializer :: Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm [[Syntax.VariableInitializer]]
unArrayInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ArrayInitializer")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Block
unBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.BlockStatement]
unBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Block")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.BreakStatement
unBreakStatement :: Phantoms.TTerm Syntax.BreakStatement -> Phantoms.TTerm (Maybe Syntax.Identifier)
unBreakStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.BreakStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.CaseConstant
unCaseConstant :: Phantoms.TTerm Syntax.CaseConstant -> Phantoms.TTerm Syntax.ConditionalExpression
unCaseConstant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.CaseConstant")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Catches
unCatches :: Phantoms.TTerm Syntax.Catches -> Phantoms.TTerm [Syntax.CatchClause]
unCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Catches")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ClassBody
unClassBody :: Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm [Syntax.ClassBodyDeclarationWithComments]
unClassBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ClassBody")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConditionalAndExpression
unConditionalAndExpression :: Phantoms.TTerm Syntax.ConditionalAndExpression -> Phantoms.TTerm [Syntax.InclusiveOrExpression]
unConditionalAndExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConditionalAndExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConditionalOrExpression
unConditionalOrExpression :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm [Syntax.ConditionalAndExpression]
unConditionalOrExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConditionalOrExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ConstantExpression
unConstantExpression :: Phantoms.TTerm Syntax.ConstantExpression -> Phantoms.TTerm Syntax.Expression
unConstantExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ConstantExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ContinueStatement
unContinueStatement :: Phantoms.TTerm Syntax.ContinueStatement -> Phantoms.TTerm (Maybe Syntax.Identifier)
unContinueStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ContinueStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.DefaultValue
unDefaultValue :: Phantoms.TTerm Syntax.DefaultValue -> Phantoms.TTerm Syntax.ElementValue
unDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.DefaultValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Dims
unDims :: Phantoms.TTerm Syntax.Dims -> Phantoms.TTerm [[Syntax.Annotation]]
unDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Dims")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ElementValueArrayInitializer
unElementValueArrayInitializer :: Phantoms.TTerm Syntax.ElementValueArrayInitializer -> Phantoms.TTerm [Syntax.ElementValue]
unElementValueArrayInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ElementValueArrayInitializer")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.EnumBody
unEnumBody :: Phantoms.TTerm Syntax.EnumBody -> Phantoms.TTerm [Syntax.EnumBody_Element]
unEnumBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.EnumBody")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.EnumConstantModifier
unEnumConstantModifier :: Phantoms.TTerm Syntax.EnumConstantModifier -> Phantoms.TTerm Syntax.Annotation
unEnumConstantModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.EnumConstantModifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ExclusiveOrExpression
unExclusiveOrExpression :: Phantoms.TTerm Syntax.ExclusiveOrExpression -> Phantoms.TTerm [Syntax.AndExpression]
unExclusiveOrExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ExclusiveOrExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ExpressionStatement
unExpressionStatement :: Phantoms.TTerm Syntax.ExpressionStatement -> Phantoms.TTerm Syntax.StatementExpression
unExpressionStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ExpressionStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Finally
unFinally :: Phantoms.TTerm Syntax.Finally -> Phantoms.TTerm Syntax.Block
unFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Finally")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.FloatingPointLiteral
unFloatingPointLiteral :: Phantoms.TTerm Syntax.FloatingPointLiteral -> Phantoms.TTerm Double
unFloatingPointLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.FloatingPointLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ForUpdate
unForUpdate :: Phantoms.TTerm Syntax.ForUpdate -> Phantoms.TTerm [Syntax.StatementExpression]
unForUpdate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ForUpdate")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Guard
unGuard :: Phantoms.TTerm Syntax.Guard -> Phantoms.TTerm Syntax.Expression
unGuard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Guard")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Identifier
unIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm String
unIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Identifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InclusiveOrExpression
unInclusiveOrExpression :: Phantoms.TTerm Syntax.InclusiveOrExpression -> Phantoms.TTerm [Syntax.ExclusiveOrExpression]
unInclusiveOrExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InclusiveOrExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InstanceInitializer
unInstanceInitializer :: Phantoms.TTerm Syntax.InstanceInitializer -> Phantoms.TTerm Syntax.Block
unInstanceInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InstanceInitializer")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.IntegerLiteral
unIntegerLiteral :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer
unIntegerLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.IntegerLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InterfaceBody
unInterfaceBody :: Phantoms.TTerm Syntax.InterfaceBody -> Phantoms.TTerm [Syntax.InterfaceMemberDeclarationWithComments]
unInterfaceBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InterfaceBody")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.InterfaceType
unInterfaceType :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.ClassType
unInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.InterfaceType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.LocalVariableDeclarationStatement
unLocalVariableDeclarationStatement :: Phantoms.TTerm Syntax.LocalVariableDeclarationStatement -> Phantoms.TTerm Syntax.LocalVariableDeclaration
unLocalVariableDeclarationStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.LocalVariableDeclarationStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MarkerAnnotation
unMarkerAnnotation :: Phantoms.TTerm Syntax.MarkerAnnotation -> Phantoms.TTerm Syntax.TypeName
unMarkerAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MarkerAnnotation")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MethodName
unMethodName :: Phantoms.TTerm Syntax.MethodName -> Phantoms.TTerm Syntax.Identifier
unMethodName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MethodName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.MethodReference_Array
unMethodReference_Array :: Phantoms.TTerm Syntax.MethodReference_Array -> Phantoms.TTerm Syntax.ArrayType
unMethodReference_Array x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.MethodReference_Array")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageModifier
unPackageModifier :: Phantoms.TTerm Syntax.PackageModifier -> Phantoms.TTerm Syntax.Annotation
unPackageModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageModifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageName
unPackageName :: Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm [Syntax.Identifier]
unPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PackageOrTypeName
unPackageOrTypeName :: Phantoms.TTerm Syntax.PackageOrTypeName -> Phantoms.TTerm [Syntax.Identifier]
unPackageOrTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PackageOrTypeName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PostDecrementExpression
unPostDecrementExpression :: Phantoms.TTerm Syntax.PostDecrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
unPostDecrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PostDecrementExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PostIncrementExpression
unPostIncrementExpression :: Phantoms.TTerm Syntax.PostIncrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
unPostIncrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PostIncrementExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PreDecrementExpression
unPreDecrementExpression :: Phantoms.TTerm Syntax.PreDecrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unPreDecrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PreDecrementExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.PreIncrementExpression
unPreIncrementExpression :: Phantoms.TTerm Syntax.PreIncrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unPreIncrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.PreIncrementExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordBody
unRecordBody :: Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm [Syntax.RecordBodyDeclaration]
unRecordBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordBody")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordComponentModifier
unRecordComponentModifier :: Phantoms.TTerm Syntax.RecordComponentModifier -> Phantoms.TTerm Syntax.Annotation
unRecordComponentModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordComponentModifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.RecordHeader
unRecordHeader :: Phantoms.TTerm Syntax.RecordHeader -> Phantoms.TTerm [Syntax.RecordComponent]
unRecordHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.RecordHeader")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ResourceSpecification
unResourceSpecification :: Phantoms.TTerm Syntax.ResourceSpecification -> Phantoms.TTerm [Syntax.Resource]
unResourceSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ResourceSpecification")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ReturnStatement
unReturnStatement :: Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
unReturnStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ReturnStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.SimpleTypeName
unSimpleTypeName :: Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm Syntax.TypeIdentifier
unSimpleTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.SimpleTypeName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.SingleTypeImportDeclaration
unSingleTypeImportDeclaration :: Phantoms.TTerm Syntax.SingleTypeImportDeclaration -> Phantoms.TTerm Syntax.TypeName
unSingleTypeImportDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.SingleTypeImportDeclaration")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StaticImportOnDemandDeclaration
unStaticImportOnDemandDeclaration :: Phantoms.TTerm Syntax.StaticImportOnDemandDeclaration -> Phantoms.TTerm Syntax.TypeName
unStaticImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StaticImportOnDemandDeclaration")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StaticInitializer
unStaticInitializer :: Phantoms.TTerm Syntax.StaticInitializer -> Phantoms.TTerm Syntax.Block
unStaticInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StaticInitializer")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.StringLiteral
unStringLiteral :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm String
unStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.StringLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TextBlock
unTextBlock :: Phantoms.TTerm Syntax.TextBlock -> Phantoms.TTerm String
unTextBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TextBlock")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.ThrowStatement
unThrowStatement :: Phantoms.TTerm Syntax.ThrowStatement -> Phantoms.TTerm Syntax.Expression
unThrowStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.ThrowStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.Throws
unThrows :: Phantoms.TTerm Syntax.Throws -> Phantoms.TTerm [Syntax.ExceptionType]
unThrows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.Throws")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeIdentifier
unTypeIdentifier :: Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.Identifier
unTypeIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeIdentifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeImportOnDemandDeclaration
unTypeImportOnDemandDeclaration :: Phantoms.TTerm Syntax.TypeImportOnDemandDeclaration -> Phantoms.TTerm Syntax.PackageOrTypeName
unTypeImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeImportOnDemandDeclaration")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypeParameterModifier
unTypeParameterModifier :: Phantoms.TTerm Syntax.TypeParameterModifier -> Phantoms.TTerm Syntax.Annotation
unTypeParameterModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypeParameterModifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.TypePattern
unTypePattern :: Phantoms.TTerm Syntax.TypePattern -> Phantoms.TTerm Syntax.LocalVariableDeclaration
unTypePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.TypePattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.UnannClassType
unUnannClassType :: Phantoms.TTerm Syntax.UnannClassType -> Phantoms.TTerm Syntax.ClassType
unUnannClassType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.UnannClassType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.UnannType
unUnannType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.Type
unUnannType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.UnannType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.java.syntax.YieldStatement
unYieldStatement :: Phantoms.TTerm Syntax.YieldStatement -> Phantoms.TTerm Syntax.Expression
unYieldStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.java.syntax.YieldStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.UnannClassType wrapper
unannClassType :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.UnannClassType
unannClassType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.UnannClassType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.java.syntax.UnannType wrapper
unannType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.UnannType
unannType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.UnannType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the minus variant of hydra.java.syntax.UnaryExpression
unaryExpressionMinus :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionMinus x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the cast variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusCast :: Phantoms.TTerm Syntax.CastExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusCast x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the not variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusNot :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusNot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the postfix variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusPostfix :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusPostfix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the switchExpression variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusSwitchExpression :: Phantoms.TTerm Syntax.SwitchExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusSwitchExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switchExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tilde variant of hydra.java.syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusTilde :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusTilde x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tilde"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the other variant of hydra.java.syntax.UnaryExpression
unaryExpressionOther :: Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the plus variant of hydra.java.syntax.UnaryExpression
unaryExpressionPlus :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionPlus x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the preDecrement variant of hydra.java.syntax.UnaryExpression
unaryExpressionPreDecrement :: Phantoms.TTerm Syntax.PreDecrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionPreDecrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the preIncrement variant of hydra.java.syntax.UnaryExpression
unaryExpressionPreIncrement :: Phantoms.TTerm Syntax.PreIncrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionPreIncrement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression typeArguments classOrInterface arguments body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Phantoms.unTTerm classOrInterface)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the arguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.Expression]
unqualifiedClassInstanceCreationExpressionArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionBody :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassBody)
unqualifiedClassInstanceCreationExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the classOrInterface field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionClassOrInterface :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
unqualifiedClassInstanceCreationExpressionClassOrInterface x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "classOrInterface")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionTypeArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.TypeArgument]
unqualifiedClassInstanceCreationExpressionTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "classOrInterface")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithBody :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "classOrInterface")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the classOrInterface field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithClassOrInterface :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithClassOrInterface original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.java.syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithTypeArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "classOrInterface")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the expressionName variant of hydra.java.syntax.VariableAccess
variableAccessExpressionName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.VariableAccess
variableAccessExpressionName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.java.syntax.VariableAccess
variableAccessFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.VariableAccess
variableAccessFieldAccess x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.java.syntax.VariableArityParameter
variableArityParameter :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameter modifiers type_ annotations identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.VariableArityParameter
variableArityParameterAnnotations :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm [Syntax.Annotation]
variableArityParameterAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableArityParameter
variableArityParameterIdentifier :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.Identifier
variableArityParameterIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.VariableArityParameter
variableArityParameterModifiers :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm [Syntax.VariableModifier]
variableArityParameterModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.VariableArityParameter
variableArityParameterType :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.UnannType
variableArityParameterType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithAnnotations :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithIdentifier :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithModifiers :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.VariableArityParameter
variableArityParameterWithType :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponent :: Phantoms.TTerm [Syntax.RecordComponentModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableArityRecordComponent
variableArityRecordComponent modifiers type_ annotations identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentAnnotations :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm [Syntax.Annotation]
variableArityRecordComponentAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentIdentifier :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm Syntax.Identifier
variableArityRecordComponentIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modifiers field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentModifiers :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm [Syntax.RecordComponentModifier]
variableArityRecordComponentModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionField = (Core.Name "modifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentType :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm Syntax.UnannType
variableArityRecordComponentType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithAnnotations :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.VariableArityRecordComponent
variableArityRecordComponentWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithIdentifier :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableArityRecordComponent
variableArityRecordComponentWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the modifiers field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithModifiers :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm [Syntax.RecordComponentModifier] -> Phantoms.TTerm Syntax.VariableArityRecordComponent
variableArityRecordComponentWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.java.syntax.VariableArityRecordComponent
variableArityRecordComponentWithType :: Phantoms.TTerm Syntax.VariableArityRecordComponent -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.VariableArityRecordComponent
variableArityRecordComponentWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "modifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableArityRecordComponent"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.VariableDeclarator
variableDeclarator :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm (Maybe Syntax.VariableInitializer) -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclarator id initializer =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Phantoms.unTTerm initializer)}]}))
-- | DSL accessor for the id field of hydra.java.syntax.VariableDeclarator
variableDeclaratorId :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.VariableDeclaratorId
variableDeclaratorId2 :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorId2 identifier dims =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)}]}))
-- | DSL accessor for the dims field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdDims :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm (Maybe Syntax.Dims)
variableDeclaratorIdDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
        Core.projectionField = (Core.Name "dims")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the identifier field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdIdentifier :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.Identifier
variableDeclaratorIdIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
        Core.projectionField = (Core.Name "identifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the dims field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdWithDims :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorIdWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
              Core.projectionField = (Core.Name "identifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the identifier field of hydra.java.syntax.VariableDeclaratorId
variableDeclaratorIdWithIdentifier :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorIdWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclaratorId"),
              Core.projectionField = (Core.Name "dims")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the initializer field of hydra.java.syntax.VariableDeclarator
variableDeclaratorInitializer :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm (Maybe Syntax.VariableInitializer)
variableDeclaratorInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
        Core.projectionField = (Core.Name "initializer")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.java.syntax.VariableDeclarator
variableDeclaratorWithId :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclaratorWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
              Core.projectionField = (Core.Name "initializer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the initializer field of hydra.java.syntax.VariableDeclarator
variableDeclaratorWithInitializer :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm (Maybe Syntax.VariableInitializer) -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclaratorWithInitializer original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.VariableDeclarator"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the arrayInitializer variant of hydra.java.syntax.VariableInitializer
variableInitializerArrayInitializer :: Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.VariableInitializer
variableInitializerArrayInitializer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.java.syntax.VariableInitializer
variableInitializerExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.VariableInitializer
variableInitializerExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the annotation variant of hydra.java.syntax.VariableModifier
variableModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.VariableModifier
variableModifierAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the final variant of hydra.java.syntax.VariableModifier
variableModifierFinal :: Phantoms.TTerm Syntax.VariableModifier
variableModifierFinal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.java.syntax.WhileStatement
whileStatement :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatement cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.WhileStatement
whileStatementBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement
whileStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.WhileStatement
whileStatementCond :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
whileStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIf :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.WhileStatementNoShortIf
whileStatementNoShortIf cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfBody :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
whileStatementNoShortIfBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfCond :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression)
whileStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithBody :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatementNoShortIf"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.java.syntax.WhileStatement
whileStatementWithBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.java.syntax.WhileStatement
whileStatementWithCond :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.syntax.Wildcard
wildcard :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm (Maybe Syntax.WildcardBounds) -> Phantoms.TTerm Syntax.Wildcard
wildcard annotations wildcard =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Phantoms.unTTerm wildcard)}]}))
-- | DSL accessor for the annotations field of hydra.java.syntax.Wildcard
wildcardAnnotations :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm [Syntax.Annotation]
wildcardAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the extends variant of hydra.java.syntax.WildcardBounds
wildcardBoundsExtends :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.WildcardBounds
wildcardBoundsExtends x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extends"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the super variant of hydra.java.syntax.WildcardBounds
wildcardBoundsSuper :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.WildcardBounds
wildcardBoundsSuper x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the wildcard field of hydra.java.syntax.Wildcard
wildcardWildcard :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm (Maybe Syntax.WildcardBounds)
wildcardWildcard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
        Core.projectionField = (Core.Name "wildcard")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotations field of hydra.java.syntax.Wildcard
wildcardWithAnnotations :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Wildcard
wildcardWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
              Core.projectionField = (Core.Name "wildcard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the wildcard field of hydra.java.syntax.Wildcard
wildcardWithWildcard :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm (Maybe Syntax.WildcardBounds) -> Phantoms.TTerm Syntax.Wildcard
wildcardWithWildcard original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.syntax.Wildcard"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.java.syntax.YieldStatement wrapper
yieldStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.YieldStatement
yieldStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.java.syntax.YieldStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
