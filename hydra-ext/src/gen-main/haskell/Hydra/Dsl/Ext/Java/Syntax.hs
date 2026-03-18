-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.java.syntax

module Hydra.Dsl.Ext.Java.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

identifier :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Identifier
identifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.Identifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm String
unIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.Identifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.TypeIdentifier
typeIdentifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.TypeIdentifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTypeIdentifier :: Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.Identifier
unTypeIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.TypeIdentifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

literalNull :: Phantoms.TTerm Syntax.Literal
literalNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

literalInteger :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalFloatingPoint :: Phantoms.TTerm Syntax.FloatingPointLiteral -> Phantoms.TTerm Syntax.Literal
literalFloatingPoint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalCharacter :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Literal
literalCharacter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerLiteral :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.IntegerLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIntegerLiteral :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer
unIntegerLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.IntegerLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

floatingPointLiteral :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatingPointLiteral
floatingPointLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.FloatingPointLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFloatingPointLiteral :: Phantoms.TTerm Syntax.FloatingPointLiteral -> Phantoms.TTerm Double
unFloatingPointLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.FloatingPointLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteral
stringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.StringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unStringLiteral :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm String
unStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.StringLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typePrimitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.Type
typePrimitive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeReference :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.Type
typeReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primitiveTypeWithAnnotations :: Phantoms.TTerm Syntax.PrimitiveType -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotations type_ annotations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)}]}))

primitiveTypeWithAnnotationsType :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.PrimitiveType
primitiveTypeWithAnnotationsType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

primitiveTypeWithAnnotationsAnnotations :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.Annotation]
primitiveTypeWithAnnotationsAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveTypeWithAnnotations"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

primitiveTypeWithAnnotationsWithType :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.PrimitiveType -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveTypeWithAnnotations"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

primitiveTypeWithAnnotationsWithAnnotations :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotationsWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveTypeWithAnnotations"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveTypeWithAnnotations"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

primitiveTypeNumeric :: Phantoms.TTerm Syntax.NumericType -> Phantoms.TTerm Syntax.PrimitiveType
primitiveTypeNumeric x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primitiveTypeBoolean :: Phantoms.TTerm Syntax.PrimitiveType
primitiveTypeBoolean =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimitiveType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

numericTypeIntegral :: Phantoms.TTerm Syntax.IntegralType -> Phantoms.TTerm Syntax.NumericType
numericTypeIntegral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericTypeFloatingPoint :: Phantoms.TTerm Syntax.FloatingPointType -> Phantoms.TTerm Syntax.NumericType
numericTypeFloatingPoint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floatingPoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integralTypeByte :: Phantoms.TTerm Syntax.IntegralType
integralTypeByte =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = Core.TermUnit}}))

integralTypeShort :: Phantoms.TTerm Syntax.IntegralType
integralTypeShort =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = Core.TermUnit}}))

integralTypeInt :: Phantoms.TTerm Syntax.IntegralType
integralTypeInt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))

integralTypeLong :: Phantoms.TTerm Syntax.IntegralType
integralTypeLong =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = Core.TermUnit}}))

integralTypeChar :: Phantoms.TTerm Syntax.IntegralType
integralTypeChar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.IntegralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = Core.TermUnit}}))

floatingPointTypeFloat :: Phantoms.TTerm Syntax.FloatingPointType
floatingPointTypeFloat =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))

floatingPointTypeDouble :: Phantoms.TTerm Syntax.FloatingPointType
floatingPointTypeDouble =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FloatingPointType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))

referenceTypeClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeClassOrInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

referenceTypeVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

referenceTypeArray :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.ReferenceType
referenceTypeArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ReferenceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classOrInterfaceTypeClass :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ClassOrInterfaceType
classOrInterfaceTypeClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classOrInterfaceTypeInterface :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.ClassOrInterfaceType
classOrInterfaceTypeInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classType :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassTypeQualifier -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ClassType
classType annotations qualifier identifier arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
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

classTypeAnnotations :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.Annotation]
classTypeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classTypeQualifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "qualifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classTypeIdentifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.TypeIdentifier
classTypeIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classTypeArguments :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.TypeArgument]
classTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classTypeWithAnnotations :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassType
classTypeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classTypeWithQualifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ClassTypeQualifier -> Phantoms.TTerm Syntax.ClassType
classTypeWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classTypeWithIdentifier :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.ClassType
classTypeWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classTypeWithArguments :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ClassType
classTypeWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassType"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classTypeQualifierNone :: Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifierNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

classTypeQualifierPackage :: Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifierPackage x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "package"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classTypeQualifierParent :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ClassTypeQualifier
classTypeQualifierParent x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassTypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceType :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.InterfaceType
interfaceType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unInterfaceType :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.ClassType
unInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.InterfaceType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariable :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeVariable
typeVariable annotations identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))

typeVariableAnnotations :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm [Syntax.Annotation]
typeVariableAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeVariable"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariableIdentifier :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.TypeIdentifier
typeVariableIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeVariable"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariableWithAnnotations :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.TypeVariable
typeVariableWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeVariable"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeVariableWithIdentifier :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeVariable
typeVariableWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeVariable"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayType :: Phantoms.TTerm Syntax.Dims -> Phantoms.TTerm Syntax.ArrayType_Variant -> Phantoms.TTerm Syntax.ArrayType
arrayType dims variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))

arrayTypeDims :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Dims
arrayTypeDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType"),
        Core.projectionField = (Core.Name "dims")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayTypeVariant :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayTypeVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType"),
        Core.projectionField = (Core.Name "variant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayTypeWithDims :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Dims -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayTypeWithVariant :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.ArrayType_Variant -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayType_VariantPrimitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayType_VariantPrimitive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayType_VariantClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayType_VariantClassOrInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayType_VariantVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.ArrayType_Variant
arrayType_VariantVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayType_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dims :: Phantoms.TTerm [[Syntax.Annotation]] -> Phantoms.TTerm Syntax.Dims
dims x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.Dims"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDims :: Phantoms.TTerm Syntax.Dims -> Phantoms.TTerm [[Syntax.Annotation]]
unDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.Dims")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeParameter :: Phantoms.TTerm [Syntax.TypeParameterModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm (Maybe Syntax.TypeBound) -> Phantoms.TTerm Syntax.TypeParameter
typeParameter modifiers identifier bound =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
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

typeParameterModifiers :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm [Syntax.TypeParameterModifier]
typeParameterModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeParameterIdentifier :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm Syntax.TypeIdentifier
typeParameterIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeParameterBound :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeBound)
typeParameterBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "bound")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeParameterWithModifiers :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm [Syntax.TypeParameterModifier] -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeParameterWithIdentifier :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeParameterWithBound :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeBound) -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeParameterModifier :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.TypeParameterModifier
typeParameterModifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.TypeParameterModifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTypeParameterModifier :: Phantoms.TTerm Syntax.TypeParameterModifier -> Phantoms.TTerm Syntax.Annotation
unTypeParameterModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.TypeParameterModifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBoundVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.TypeBound
typeBoundVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeBoundClassOrInterface :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm Syntax.TypeBound
typeBoundClassOrInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeBound_ClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterface type_ additional =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Phantoms.unTTerm additional)}]}))

typeBound_ClassOrInterfaceType :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType
typeBound_ClassOrInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBound_ClassOrInterfaceAdditional :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm [Syntax.AdditionalBound]
typeBound_ClassOrInterfaceAdditional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound_ClassOrInterface"),
        Core.projectionField = (Core.Name "additional")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBound_ClassOrInterfaceWithType :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound_ClassOrInterface"),
              Core.projectionField = (Core.Name "additional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeBound_ClassOrInterfaceWithAdditional :: Phantoms.TTerm Syntax.TypeBound_ClassOrInterface -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.TypeBound_ClassOrInterface
typeBound_ClassOrInterfaceWithAdditional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeBound_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "additional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

additionalBound :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.AdditionalBound
additionalBound x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.AdditionalBound"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAdditionalBound :: Phantoms.TTerm Syntax.AdditionalBound -> Phantoms.TTerm Syntax.InterfaceType
unAdditionalBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.AdditionalBound")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeArgumentReference :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.TypeArgument
typeArgumentReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeArgumentWildcard :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm Syntax.TypeArgument
typeArgumentWildcard x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

wildcard :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm (Maybe Syntax.WildcardBounds) -> Phantoms.TTerm Syntax.Wildcard
wildcard annotations wildcard =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Phantoms.unTTerm wildcard)}]}))

wildcardAnnotations :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm [Syntax.Annotation]
wildcardAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Wildcard"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wildcardWildcard :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm (Maybe Syntax.WildcardBounds)
wildcardWildcard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Wildcard"),
        Core.projectionField = (Core.Name "wildcard")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wildcardWithAnnotations :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Wildcard
wildcardWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Wildcard"),
              Core.projectionField = (Core.Name "wildcard")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

wildcardWithWildcard :: Phantoms.TTerm Syntax.Wildcard -> Phantoms.TTerm (Maybe Syntax.WildcardBounds) -> Phantoms.TTerm Syntax.Wildcard
wildcardWithWildcard original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Wildcard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Wildcard"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

wildcardBoundsExtends :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.WildcardBounds
wildcardBoundsExtends x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extends"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

wildcardBoundsSuper :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.WildcardBounds
wildcardBoundsSuper x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.WildcardBounds"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleName :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm Syntax.ModuleName
moduleName identifier name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

moduleNameIdentifier :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Identifier
moduleNameIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleName"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleNameName :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm (Maybe Syntax.ModuleName)
moduleNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleName"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleNameWithIdentifier :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ModuleName
moduleNameWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleName"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleNameWithName :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm Syntax.ModuleName
moduleNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleName"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

packageName :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageName
packageName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.PackageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPackageName :: Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm [Syntax.Identifier]
unPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.PackageName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeName :: Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm (Maybe Syntax.PackageOrTypeName) -> Phantoms.TTerm Syntax.TypeName
typeName identifier qualifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)}]}))

typeNameIdentifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.TypeIdentifier
typeNameIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeName"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeNameQualifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.PackageOrTypeName)
typeNameQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeName"),
        Core.projectionField = (Core.Name "qualifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeNameWithIdentifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.TypeName
typeNameWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeName"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeNameWithQualifier :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.PackageOrTypeName) -> Phantoms.TTerm Syntax.TypeName
typeNameWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeName"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

expressionName :: Phantoms.TTerm (Maybe Syntax.AmbiguousName) -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ExpressionName
expressionName qualifier identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))

expressionNameQualifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm (Maybe Syntax.AmbiguousName)
expressionNameQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionName"),
        Core.projectionField = (Core.Name "qualifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

expressionNameIdentifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.Identifier
expressionNameIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionName"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

expressionNameWithQualifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm (Maybe Syntax.AmbiguousName) -> Phantoms.TTerm Syntax.ExpressionName
expressionNameWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionName"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

expressionNameWithIdentifier :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ExpressionName
expressionNameWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionName"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodName :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodName
methodName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.MethodName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMethodName :: Phantoms.TTerm Syntax.MethodName -> Phantoms.TTerm Syntax.Identifier
unMethodName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.MethodName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageOrTypeName :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageOrTypeName
packageOrTypeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.PackageOrTypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPackageOrTypeName :: Phantoms.TTerm Syntax.PackageOrTypeName -> Phantoms.TTerm [Syntax.Identifier]
unPackageOrTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.PackageOrTypeName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ambiguousName :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.AmbiguousName
ambiguousName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.AmbiguousName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAmbiguousName :: Phantoms.TTerm Syntax.AmbiguousName -> Phantoms.TTerm [Syntax.Identifier]
unAmbiguousName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.AmbiguousName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compilationUnitOrdinary :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm Syntax.CompilationUnit
compilationUnitOrdinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

compilationUnitModular :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm Syntax.CompilationUnit
compilationUnitModular x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.CompilationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modular"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ordinaryCompilationUnit :: Phantoms.TTerm (Maybe Syntax.PackageDeclaration) -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm [Syntax.TypeDeclarationWithComments] -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnit package imports types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
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

ordinaryCompilationUnitPackage :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm (Maybe Syntax.PackageDeclaration)
ordinaryCompilationUnitPackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionField = (Core.Name "package")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ordinaryCompilationUnitImports :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration]
ordinaryCompilationUnitImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionField = (Core.Name "imports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ordinaryCompilationUnitTypes :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.TypeDeclarationWithComments]
ordinaryCompilationUnitTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ordinaryCompilationUnitWithPackage :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm (Maybe Syntax.PackageDeclaration) -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ordinaryCompilationUnitWithImports :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "package")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ordinaryCompilationUnitWithTypes :: Phantoms.TTerm Syntax.OrdinaryCompilationUnit -> Phantoms.TTerm [Syntax.TypeDeclarationWithComments] -> Phantoms.TTerm Syntax.OrdinaryCompilationUnit
ordinaryCompilationUnitWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "package")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.OrdinaryCompilationUnit"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

modularCompilationUnit :: Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.ModularCompilationUnit
modularCompilationUnit imports module_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)}]}))

modularCompilationUnitImports :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration]
modularCompilationUnitImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModularCompilationUnit"),
        Core.projectionField = (Core.Name "imports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

modularCompilationUnitModule :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm Syntax.ModuleDeclaration
modularCompilationUnitModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModularCompilationUnit"),
        Core.projectionField = (Core.Name "module")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

modularCompilationUnitWithImports :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.ModularCompilationUnit
modularCompilationUnitWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModularCompilationUnit"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

modularCompilationUnitWithModule :: Phantoms.TTerm Syntax.ModularCompilationUnit -> Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.ModularCompilationUnit
modularCompilationUnitWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModularCompilationUnit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModularCompilationUnit"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

packageDeclaration :: Phantoms.TTerm [Syntax.PackageModifier] -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageDeclaration
packageDeclaration modifiers identifiers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm identifiers)}]}))

packageDeclarationModifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.PackageModifier]
packageDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PackageDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageDeclarationIdentifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.Identifier]
packageDeclarationIdentifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PackageDeclaration"),
        Core.projectionField = (Core.Name "identifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

packageDeclarationWithModifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.PackageModifier] -> Phantoms.TTerm Syntax.PackageDeclaration
packageDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PackageDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

packageDeclarationWithIdentifiers :: Phantoms.TTerm Syntax.PackageDeclaration -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.PackageDeclaration
packageDeclarationWithIdentifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.PackageDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.PackageDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

packageModifier :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.PackageModifier
packageModifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.PackageModifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPackageModifier :: Phantoms.TTerm Syntax.PackageModifier -> Phantoms.TTerm Syntax.Annotation
unPackageModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.PackageModifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDeclarationSingleType :: Phantoms.TTerm Syntax.SingleTypeImportDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationSingleType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importDeclarationTypeImportOnDemand :: Phantoms.TTerm Syntax.TypeImportOnDemandDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationTypeImportOnDemand x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeImportOnDemand"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importDeclarationSingleStaticImport :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationSingleStaticImport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleStaticImport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importDeclarationStaticImportOnDemand :: Phantoms.TTerm Syntax.StaticImportOnDemandDeclaration -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationStaticImportOnDemand x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ImportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticImportOnDemand"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

singleTypeImportDeclaration :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.SingleTypeImportDeclaration
singleTypeImportDeclaration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.SingleTypeImportDeclaration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSingleTypeImportDeclaration :: Phantoms.TTerm Syntax.SingleTypeImportDeclaration -> Phantoms.TTerm Syntax.TypeName
unSingleTypeImportDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.SingleTypeImportDeclaration")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeImportOnDemandDeclaration :: Phantoms.TTerm Syntax.PackageOrTypeName -> Phantoms.TTerm Syntax.TypeImportOnDemandDeclaration
typeImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.TypeImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTypeImportOnDemandDeclaration :: Phantoms.TTerm Syntax.TypeImportOnDemandDeclaration -> Phantoms.TTerm Syntax.PackageOrTypeName
unTypeImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.TypeImportOnDemandDeclaration")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleStaticImportDeclaration :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SingleStaticImportDeclaration
singleStaticImportDeclaration typeName identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))

singleStaticImportDeclarationTypeName :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.TypeName
singleStaticImportDeclarationTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleStaticImportDeclarationIdentifier :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.Identifier
singleStaticImportDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleStaticImportDeclaration"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleStaticImportDeclarationWithTypeName :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleStaticImportDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

singleStaticImportDeclarationWithIdentifier :: Phantoms.TTerm Syntax.SingleStaticImportDeclaration -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SingleStaticImportDeclaration
singleStaticImportDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SingleStaticImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleStaticImportDeclaration"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

staticImportOnDemandDeclaration :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.StaticImportOnDemandDeclaration
staticImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.StaticImportOnDemandDeclaration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unStaticImportOnDemandDeclaration :: Phantoms.TTerm Syntax.StaticImportOnDemandDeclaration -> Phantoms.TTerm Syntax.TypeName
unStaticImportOnDemandDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.StaticImportOnDemandDeclaration")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.TypeDeclaration
typeDeclarationClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.TypeDeclaration
typeDeclarationInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDeclarationNone :: Phantoms.TTerm Syntax.TypeDeclaration
typeDeclarationNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

typeDeclarationWithComments :: Phantoms.TTerm Syntax.TypeDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeDeclarationWithComments
typeDeclarationWithComments value comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

typeDeclarationWithCommentsValue :: Phantoms.TTerm Syntax.TypeDeclarationWithComments -> Phantoms.TTerm Syntax.TypeDeclaration
typeDeclarationWithCommentsValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclarationWithComments"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclarationWithCommentsComments :: Phantoms.TTerm Syntax.TypeDeclarationWithComments -> Phantoms.TTerm (Maybe String)
typeDeclarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclarationWithCommentsWithValue :: Phantoms.TTerm Syntax.TypeDeclarationWithComments -> Phantoms.TTerm Syntax.TypeDeclaration -> Phantoms.TTerm Syntax.TypeDeclarationWithComments
typeDeclarationWithCommentsWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDeclarationWithCommentsWithComments :: Phantoms.TTerm Syntax.TypeDeclarationWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeDeclarationWithComments
typeDeclarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeDeclarationWithComments"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleDeclaration :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm [[Syntax.ModuleDirective]] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclaration annotations open identifiers directives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
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

moduleDeclarationAnnotations :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Annotation]
moduleDeclarationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDeclarationOpen :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Bool
moduleDeclarationOpen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "open")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDeclarationIdentifiers :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Identifier]
moduleDeclarationIdentifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "identifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDeclarationDirectives :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [[Syntax.ModuleDirective]]
moduleDeclarationDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "directives")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDeclarationWithAnnotations :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "open")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "directives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDeclarationWithOpen :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithOpen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "directives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDeclarationWithIdentifiers :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithIdentifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "open")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "directives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDeclarationWithDirectives :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm [[Syntax.ModuleDirective]] -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "open")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "identifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleDirectiveRequires :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveRequires x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "requires"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleDirectiveExports :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveExports x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exports"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleDirectiveOpens :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveOpens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleDirectiveUses :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveUses x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uses"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleDirectiveProvides :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm Syntax.ModuleDirective
moduleDirectiveProvides x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "provides"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleDirective_Requires :: Phantoms.TTerm [Syntax.RequiresModifier] -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.ModuleDirective_Requires
moduleDirective_Requires modifiers module_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)}]}))

moduleDirective_RequiresModifiers :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm [Syntax.RequiresModifier]
moduleDirective_RequiresModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Requires"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDirective_RequiresModule :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm Syntax.ModuleName
moduleDirective_RequiresModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Requires"),
        Core.projectionField = (Core.Name "module")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDirective_RequiresWithModifiers :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm [Syntax.RequiresModifier] -> Phantoms.TTerm Syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Requires"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDirective_RequiresWithModule :: Phantoms.TTerm Syntax.ModuleDirective_Requires -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.ModuleDirective_Requires
moduleDirective_RequiresWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Requires"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Requires"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleDirective_ExportsOrOpens :: Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm [Syntax.ModuleName] -> Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpens package modules =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm modules)}]}))

moduleDirective_ExportsOrOpensPackage :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.PackageName
moduleDirective_ExportsOrOpensPackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionField = (Core.Name "package")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDirective_ExportsOrOpensModules :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm [Syntax.ModuleName]
moduleDirective_ExportsOrOpensModules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens"),
        Core.projectionField = (Core.Name "modules")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDirective_ExportsOrOpensWithPackage :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm Syntax.PackageName -> Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens"),
              Core.projectionField = (Core.Name "modules")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDirective_ExportsOrOpensWithModules :: Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens -> Phantoms.TTerm [Syntax.ModuleName] -> Phantoms.TTerm Syntax.ModuleDirective_ExportsOrOpens
moduleDirective_ExportsOrOpensWithModules original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens"),
              Core.projectionField = (Core.Name "package")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleDirective_Provides :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.ModuleDirective_Provides
moduleDirective_Provides to with =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm to)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm with)}]}))

moduleDirective_ProvidesTo :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm Syntax.TypeName
moduleDirective_ProvidesTo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Provides"),
        Core.projectionField = (Core.Name "to")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDirective_ProvidesWith :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm [Syntax.TypeName]
moduleDirective_ProvidesWith x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Provides"),
        Core.projectionField = (Core.Name "with")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDirective_ProvidesWithTo :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithTo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Provides"),
              Core.projectionField = (Core.Name "with")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDirective_ProvidesWithWith :: Phantoms.TTerm Syntax.ModuleDirective_Provides -> Phantoms.TTerm [Syntax.TypeName] -> Phantoms.TTerm Syntax.ModuleDirective_Provides
moduleDirective_ProvidesWithWith original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Provides"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "to"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ModuleDirective_Provides"),
              Core.projectionField = (Core.Name "to")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

requiresModifierTransitive :: Phantoms.TTerm Syntax.RequiresModifier
requiresModifierTransitive =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transitive"),
        Core.fieldTerm = Core.TermUnit}}))

requiresModifierStatic :: Phantoms.TTerm Syntax.RequiresModifier
requiresModifierStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RequiresModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

classDeclarationNormal :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classDeclarationEnum :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationEnum x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

normalClassDeclaration :: Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm (Maybe Syntax.ClassType) -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclaration modifiers identifier parameters extends implements body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
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
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

normalClassDeclarationModifiers :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.ClassModifier]
normalClassDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalClassDeclarationIdentifier :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
normalClassDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalClassDeclarationParameters :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.TypeParameter]
normalClassDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalClassDeclarationExtends :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm (Maybe Syntax.ClassType)
normalClassDeclarationExtends x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "extends")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalClassDeclarationImplements :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.InterfaceType]
normalClassDeclarationImplements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "implements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalClassDeclarationBody :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.ClassBody
normalClassDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalClassDeclarationWithModifiers :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalClassDeclarationWithIdentifier :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalClassDeclarationWithParameters :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalClassDeclarationWithExtends :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm (Maybe Syntax.ClassType) -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithExtends original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalClassDeclarationWithImplements :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithImplements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalClassDeclarationWithBody :: Phantoms.TTerm Syntax.NormalClassDeclaration -> Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm Syntax.NormalClassDeclaration
normalClassDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalClassDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ClassModifier
classModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classModifierPublic :: Phantoms.TTerm Syntax.ClassModifier
classModifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

classModifierProtected :: Phantoms.TTerm Syntax.ClassModifier
classModifierProtected =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))

classModifierPrivate :: Phantoms.TTerm Syntax.ClassModifier
classModifierPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

classModifierAbstract :: Phantoms.TTerm Syntax.ClassModifier
classModifierAbstract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))

classModifierStatic :: Phantoms.TTerm Syntax.ClassModifier
classModifierStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

classModifierFinal :: Phantoms.TTerm Syntax.ClassModifier
classModifierFinal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

classModifierStrictfp :: Phantoms.TTerm Syntax.ClassModifier
classModifierStrictfp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))

classBody :: Phantoms.TTerm [Syntax.ClassBodyDeclarationWithComments] -> Phantoms.TTerm Syntax.ClassBody
classBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ClassBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unClassBody :: Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm [Syntax.ClassBodyDeclarationWithComments]
unClassBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ClassBody")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classBodyDeclarationClassMember :: Phantoms.TTerm Syntax.ClassMemberDeclaration -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationClassMember x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classMember"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classBodyDeclarationInstanceInitializer :: Phantoms.TTerm Syntax.InstanceInitializer -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationInstanceInitializer x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classBodyDeclarationStaticInitializer :: Phantoms.TTerm Syntax.StaticInitializer -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationStaticInitializer x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classBodyDeclarationConstructorDeclaration :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationConstructorDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructorDeclaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classBodyDeclarationWithComments :: Phantoms.TTerm Syntax.ClassBodyDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithComments value comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

classBodyDeclarationWithCommentsValue :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm Syntax.ClassBodyDeclaration
classBodyDeclarationWithCommentsValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classBodyDeclarationWithCommentsComments :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm (Maybe String)
classBodyDeclarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classBodyDeclarationWithCommentsWithValue :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm Syntax.ClassBodyDeclaration -> Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classBodyDeclarationWithCommentsWithComments :: Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ClassBodyDeclarationWithComments
classBodyDeclarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassBodyDeclarationWithComments"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classMemberDeclarationField :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "field"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classMemberDeclarationMethod :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classMemberDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classMemberDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classMemberDeclarationNone :: Phantoms.TTerm Syntax.ClassMemberDeclaration
classMemberDeclarationNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

fieldDeclaration :: Phantoms.TTerm [Syntax.FieldModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclaration modifiers unannType variableDeclarators =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
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

fieldDeclarationModifiers :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.FieldModifier]
fieldDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDeclarationUnannType :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm Syntax.UnannType
fieldDeclarationUnannType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
        Core.projectionField = (Core.Name "unannType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDeclarationVariableDeclarators :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator]
fieldDeclarationVariableDeclarators x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
        Core.projectionField = (Core.Name "variableDeclarators")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDeclarationWithModifiers :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.FieldModifier] -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "unannType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "variableDeclarators")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldDeclarationWithUnannType :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclarationWithUnannType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "variableDeclarators")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldDeclarationWithVariableDeclarators :: Phantoms.TTerm Syntax.FieldDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.FieldDeclaration
fieldDeclarationWithVariableDeclarators original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldDeclaration"),
              Core.projectionField = (Core.Name "unannType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variableDeclarators"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.FieldModifier
fieldModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fieldModifierPublic :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

fieldModifierProtected :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierProtected =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))

fieldModifierPrivate :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

fieldModifierStatic :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

fieldModifierFinal :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierFinal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

fieldModifierTransient :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierTransient =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transient"),
        Core.fieldTerm = Core.TermUnit}}))

fieldModifierVolatile :: Phantoms.TTerm Syntax.FieldModifier
fieldModifierVolatile =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "volatile"),
        Core.fieldTerm = Core.TermUnit}}))

variableDeclarator :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm (Maybe Syntax.VariableInitializer) -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclarator id initializer =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Phantoms.unTTerm initializer)}]}))

variableDeclaratorId :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclarator"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclaratorInitializer :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm (Maybe Syntax.VariableInitializer)
variableDeclaratorInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclarator"),
        Core.projectionField = (Core.Name "initializer")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclaratorWithId :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclaratorWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclarator"),
              Core.projectionField = (Core.Name "initializer")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDeclaratorWithInitializer :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm (Maybe Syntax.VariableInitializer) -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclaratorWithInitializer original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclarator"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableDeclaratorId_ :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorId_ identifier dims =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm dims)}]}))

variableDeclaratorIdIdentifier :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.Identifier
variableDeclaratorIdIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclaratorId"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclaratorIdDims :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm (Maybe Syntax.Dims)
variableDeclaratorIdDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclaratorId"),
        Core.projectionField = (Core.Name "dims")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclaratorIdWithIdentifier :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorIdWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclaratorId"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDeclaratorIdWithDims :: Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.VariableDeclaratorId
variableDeclaratorIdWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclaratorId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableDeclaratorId"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableInitializerExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.VariableInitializer
variableInitializerExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

variableInitializerArrayInitializer :: Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.VariableInitializer
variableInitializerArrayInitializer x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableInitializer"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unannType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.UnannType
unannType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.UnannType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unUnannType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.Type
unUnannType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.UnannType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unannClassType :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.UnannClassType
unannClassType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.UnannClassType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unUnannClassType :: Phantoms.TTerm Syntax.UnannClassType -> Phantoms.TTerm Syntax.ClassType
unUnannClassType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.UnannClassType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclaration :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.MethodModifier] -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclaration annotations modifiers header body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
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

methodDeclarationAnnotations :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.Annotation]
methodDeclarationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclarationModifiers :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.MethodModifier]
methodDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclarationHeader :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader
methodDeclarationHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "header")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclarationBody :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodBody
methodDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclarationWithAnnotations :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "header")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDeclarationWithModifiers :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm [Syntax.MethodModifier] -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "header")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDeclarationWithHeader :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDeclarationWithBody :: Phantoms.TTerm Syntax.MethodDeclaration -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.MethodDeclaration
methodDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclaration"),
              Core.projectionField = (Core.Name "header")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.MethodModifier
methodModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodModifierPublic :: Phantoms.TTerm Syntax.MethodModifier
methodModifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierProtected :: Phantoms.TTerm Syntax.MethodModifier
methodModifierProtected =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierPrivate :: Phantoms.TTerm Syntax.MethodModifier
methodModifierPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierAbstract :: Phantoms.TTerm Syntax.MethodModifier
methodModifierAbstract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierStatic :: Phantoms.TTerm Syntax.MethodModifier
methodModifierStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierFinal :: Phantoms.TTerm Syntax.MethodModifier
methodModifierFinal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierSynchronized :: Phantoms.TTerm Syntax.MethodModifier
methodModifierSynchronized =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierNative :: Phantoms.TTerm Syntax.MethodModifier
methodModifierNative =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "native"),
        Core.fieldTerm = Core.TermUnit}}))

methodModifierStrictfb :: Phantoms.TTerm Syntax.MethodModifier
methodModifierStrictfb =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfb"),
        Core.fieldTerm = Core.TermUnit}}))

methodHeader :: Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.Result -> Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.MethodHeader
methodHeader parameters result declarator throws =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
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

methodHeaderParameters :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm [Syntax.TypeParameter]
methodHeaderParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodHeaderResult :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.Result
methodHeaderResult x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "result")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodHeaderDeclarator :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodDeclarator
methodHeaderDeclarator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "declarator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodHeaderThrows :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm (Maybe Syntax.Throws)
methodHeaderThrows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
        Core.projectionField = (Core.Name "throws")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodHeaderWithParameters :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "result")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "declarator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "throws")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodHeaderWithResult :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.Result -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithResult original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "declarator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "throws")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodHeaderWithDeclarator :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithDeclarator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "result")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "throws")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodHeaderWithThrows :: Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.MethodHeader
methodHeaderWithThrows original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "result")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodHeader"),
              Core.projectionField = (Core.Name "declarator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

resultType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.Result
resultType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resultVoid :: Phantoms.TTerm Syntax.Result
resultVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))

methodDeclarator :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclarator identifier receiverParameter formalParameters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
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

methodDeclaratorIdentifier :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm Syntax.Identifier
methodDeclaratorIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclaratorReceiverParameter :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter)
methodDeclaratorReceiverParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
        Core.projectionField = (Core.Name "receiverParameter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclaratorFormalParameters :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm [Syntax.FormalParameter]
methodDeclaratorFormalParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
        Core.projectionField = (Core.Name "formalParameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDeclaratorWithIdentifier :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclaratorWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDeclaratorWithReceiverParameter :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclaratorWithReceiverParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDeclaratorWithFormalParameters :: Phantoms.TTerm Syntax.MethodDeclarator -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.MethodDeclarator
methodDeclaratorWithFormalParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

receiverParameter :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameter annotations unannType identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
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

receiverParameterAnnotations :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm [Syntax.Annotation]
receiverParameterAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

receiverParameterUnannType :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm Syntax.UnannType
receiverParameterUnannType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
        Core.projectionField = (Core.Name "unannType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

receiverParameterIdentifier :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm (Maybe Syntax.Identifier)
receiverParameterIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

receiverParameterWithAnnotations :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameterWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "unannType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

receiverParameterWithUnannType :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameterWithUnannType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

receiverParameterWithIdentifier :: Phantoms.TTerm Syntax.ReceiverParameter -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ReceiverParameter
receiverParameterWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unannType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ReceiverParameter"),
              Core.projectionField = (Core.Name "unannType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

formalParameterSimple :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.FormalParameter
formalParameterSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

formalParameterVariableArity :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.FormalParameter
formalParameterVariableArity x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

formalParameter_Simple :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_Simple modifiers type_ id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
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

formalParameter_SimpleModifiers :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm [Syntax.VariableModifier]
formalParameter_SimpleModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

formalParameter_SimpleType :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.UnannType
formalParameter_SimpleType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

formalParameter_SimpleId :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.VariableDeclaratorId
formalParameter_SimpleId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

formalParameter_SimpleWithModifiers :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_SimpleWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

formalParameter_SimpleWithType :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_SimpleWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

formalParameter_SimpleWithId :: Phantoms.TTerm Syntax.FormalParameter_Simple -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.FormalParameter_Simple
formalParameter_SimpleWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FormalParameter_Simple"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableArityParameter :: Phantoms.TTerm Syntax.VariableModifier -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameter modifiers type_ annotations identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
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

variableArityParameterModifiers :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.VariableModifier
variableArityParameterModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableArityParameterType :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.UnannType
variableArityParameterType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableArityParameterAnnotations :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm [Syntax.Annotation]
variableArityParameterAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableArityParameterIdentifier :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.Identifier
variableArityParameterIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableArityParameterWithModifiers :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.VariableModifier -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableArityParameterWithType :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableArityParameterWithAnnotations :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableArityParameterWithIdentifier :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.VariableArityParameter
variableArityParameterWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableArityParameter"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.VariableModifier
variableModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

variableModifierFinal :: Phantoms.TTerm Syntax.VariableModifier
variableModifierFinal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

throws :: Phantoms.TTerm [Syntax.ExceptionType] -> Phantoms.TTerm Syntax.Throws
throws x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.Throws"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unThrows :: Phantoms.TTerm Syntax.Throws -> Phantoms.TTerm [Syntax.ExceptionType]
unThrows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.Throws")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exceptionTypeClass :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.ExceptionType
exceptionTypeClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exceptionTypeVariable :: Phantoms.TTerm Syntax.TypeVariable -> Phantoms.TTerm Syntax.ExceptionType
exceptionTypeVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ExceptionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodBodyBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.MethodBody
methodBodyBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodBodyNone :: Phantoms.TTerm Syntax.MethodBody
methodBodyNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

instanceInitializer :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.InstanceInitializer
instanceInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.InstanceInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unInstanceInitializer :: Phantoms.TTerm Syntax.InstanceInitializer -> Phantoms.TTerm Syntax.Block
unInstanceInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.InstanceInitializer")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticInitializer :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.StaticInitializer
staticInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.StaticInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unStaticInitializer :: Phantoms.TTerm Syntax.StaticInitializer -> Phantoms.TTerm Syntax.Block
unStaticInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.StaticInitializer")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclaration :: Phantoms.TTerm [Syntax.ConstructorModifier] -> Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclaration modifiers constructor throws body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
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

constructorDeclarationModifiers :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.ConstructorModifier]
constructorDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationConstructor :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclarationConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "constructor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationThrows :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm (Maybe Syntax.Throws)
constructorDeclarationThrows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "throws")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationBody :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorBody
constructorDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationWithModifiers :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.ConstructorModifier] -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "constructor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "throws")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclarationWithConstructor :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithConstructor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "throws")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclarationWithThrows :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm (Maybe Syntax.Throws) -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithThrows original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "constructor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclarationWithBody :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "constructor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "throws"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "throws")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constructorModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constructorModifierPublic :: Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

constructorModifierProtected :: Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierProtected =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))

constructorModifierPrivate :: Phantoms.TTerm Syntax.ConstructorModifier
constructorModifierPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

constructorDeclarator :: Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclarator parameters name receiverParameter formalParameters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
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

constructorDeclaratorParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.TypeParameter]
constructorDeclaratorParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclaratorName :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm Syntax.SimpleTypeName
constructorDeclaratorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclaratorReceiverParameter :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter)
constructorDeclaratorReceiverParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "receiverParameter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclaratorFormalParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.FormalParameter]
constructorDeclaratorFormalParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
        Core.projectionField = (Core.Name "formalParameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclaratorWithParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclaratorWithName :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclaratorWithReceiverParameter :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm (Maybe Syntax.ReceiverParameter) -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithReceiverParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "formalParameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclaratorWithFormalParameters :: Phantoms.TTerm Syntax.ConstructorDeclarator -> Phantoms.TTerm [Syntax.FormalParameter] -> Phantoms.TTerm Syntax.ConstructorDeclarator
constructorDeclaratorWithFormalParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "receiverParameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorDeclarator"),
              Core.projectionField = (Core.Name "receiverParameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formalParameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

simpleTypeName :: Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.SimpleTypeName
simpleTypeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.SimpleTypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSimpleTypeName :: Phantoms.TTerm Syntax.SimpleTypeName -> Phantoms.TTerm Syntax.TypeIdentifier
unSimpleTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.SimpleTypeName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorBody :: Phantoms.TTerm (Maybe Syntax.ExplicitConstructorInvocation) -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.ConstructorBody
constructorBody invocation statements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Phantoms.unTTerm invocation)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)}]}))

constructorBodyInvocation :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm (Maybe Syntax.ExplicitConstructorInvocation)
constructorBodyInvocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorBody"),
        Core.projectionField = (Core.Name "invocation")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorBodyStatements :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm [Syntax.BlockStatement]
constructorBodyStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorBody"),
        Core.projectionField = (Core.Name "statements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorBodyWithInvocation :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm (Maybe Syntax.ExplicitConstructorInvocation) -> Phantoms.TTerm Syntax.ConstructorBody
constructorBodyWithInvocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorBody"),
              Core.projectionField = (Core.Name "statements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorBodyWithStatements :: Phantoms.TTerm Syntax.ConstructorBody -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.ConstructorBody
constructorBodyWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "invocation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstructorBody"),
              Core.projectionField = (Core.Name "invocation")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

explicitConstructorInvocation :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocation typeArguments arguments variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
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

explicitConstructorInvocationTypeArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.TypeArgument]
explicitConstructorInvocationTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitConstructorInvocationArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.Expression]
explicitConstructorInvocationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitConstructorInvocationVariant :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocationVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
        Core.projectionField = (Core.Name "variant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitConstructorInvocationWithTypeArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

explicitConstructorInvocationWithArguments :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

explicitConstructorInvocationWithVariant :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation
explicitConstructorInvocationWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

explicitConstructorInvocation_VariantThis :: Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantThis =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))

explicitConstructorInvocation_VariantSuper :: Phantoms.TTerm (Maybe Syntax.ExpressionName) -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantSuper x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

explicitConstructorInvocation_VariantPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.ExplicitConstructorInvocation_Variant
explicitConstructorInvocation_VariantPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumDeclaration :: Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.EnumBody -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclaration modifiers identifier implements body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
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

enumDeclarationModifiers :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.ClassModifier]
enumDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDeclarationIdentifier :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
enumDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDeclarationImplements :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.InterfaceType]
enumDeclarationImplements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "implements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDeclarationBody :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.EnumBody
enumDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDeclarationWithModifiers :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.ClassModifier] -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDeclarationWithIdentifier :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDeclarationWithImplements :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithImplements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDeclarationWithBody :: Phantoms.TTerm Syntax.EnumDeclaration -> Phantoms.TTerm Syntax.EnumBody -> Phantoms.TTerm Syntax.EnumDeclaration
enumDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumDeclaration"),
              Core.projectionField = (Core.Name "implements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumBody :: Phantoms.TTerm [Syntax.EnumBody_Element] -> Phantoms.TTerm Syntax.EnumBody
enumBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unEnumBody :: Phantoms.TTerm Syntax.EnumBody -> Phantoms.TTerm [Syntax.EnumBody_Element]
unEnumBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.EnumBody")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumBody_Element :: Phantoms.TTerm [Syntax.EnumConstant] -> Phantoms.TTerm [Syntax.ClassBodyDeclaration] -> Phantoms.TTerm Syntax.EnumBody_Element
enumBody_Element constants bodyDeclarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Phantoms.unTTerm constants)},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Phantoms.unTTerm bodyDeclarations)}]}))

enumBody_ElementConstants :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.EnumConstant]
enumBody_ElementConstants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody_Element"),
        Core.projectionField = (Core.Name "constants")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumBody_ElementBodyDeclarations :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.ClassBodyDeclaration]
enumBody_ElementBodyDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody_Element"),
        Core.projectionField = (Core.Name "bodyDeclarations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumBody_ElementWithConstants :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.EnumConstant] -> Phantoms.TTerm Syntax.EnumBody_Element
enumBody_ElementWithConstants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody_Element"),
              Core.projectionField = (Core.Name "bodyDeclarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumBody_ElementWithBodyDeclarations :: Phantoms.TTerm Syntax.EnumBody_Element -> Phantoms.TTerm [Syntax.ClassBodyDeclaration] -> Phantoms.TTerm Syntax.EnumBody_Element
enumBody_ElementWithBodyDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody_Element"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumBody_Element"),
              Core.projectionField = (Core.Name "constants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodyDeclarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumConstant :: Phantoms.TTerm [Syntax.EnumConstantModifier] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm [[Syntax.Expression]] -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.EnumConstant
enumConstant modifiers identifier arguments body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
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

enumConstantModifiers :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm [Syntax.EnumConstantModifier]
enumConstantModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumConstantIdentifier :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm Syntax.Identifier
enumConstantIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumConstantArguments :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm [[Syntax.Expression]]
enumConstantArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumConstantBody :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm (Maybe Syntax.ClassBody)
enumConstantBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumConstantWithModifiers :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm [Syntax.EnumConstantModifier] -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumConstantWithIdentifier :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumConstantWithArguments :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm [[Syntax.Expression]] -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumConstantWithBody :: Phantoms.TTerm Syntax.EnumConstant -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.EnumConstant
enumConstantWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstant"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumConstantModifier :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.EnumConstantModifier
enumConstantModifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstantModifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unEnumConstantModifier :: Phantoms.TTerm Syntax.EnumConstantModifier -> Phantoms.TTerm Syntax.Annotation
unEnumConstantModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.EnumConstantModifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceDeclarationNormalInterface :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceDeclaration
interfaceDeclarationNormalInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceDeclarationAnnotationType :: Phantoms.TTerm Syntax.AnnotationTypeDeclaration -> Phantoms.TTerm Syntax.InterfaceDeclaration
interfaceDeclarationAnnotationType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

normalInterfaceDeclaration :: Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.InterfaceBody -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclaration modifiers identifier parameters extends body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
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
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

normalInterfaceDeclarationModifiers :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier]
normalInterfaceDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalInterfaceDeclarationIdentifier :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
normalInterfaceDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalInterfaceDeclarationParameters :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.TypeParameter]
normalInterfaceDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalInterfaceDeclarationExtends :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceType]
normalInterfaceDeclarationExtends x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "extends")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalInterfaceDeclarationBody :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceBody
normalInterfaceDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalInterfaceDeclarationWithModifiers :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalInterfaceDeclarationWithIdentifier :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalInterfaceDeclarationWithParameters :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalInterfaceDeclarationWithExtends :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm [Syntax.InterfaceType] -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithExtends original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalInterfaceDeclarationWithBody :: Phantoms.TTerm Syntax.NormalInterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceBody -> Phantoms.TTerm Syntax.NormalInterfaceDeclaration
normalInterfaceDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalInterfaceDeclaration"),
              Core.projectionField = (Core.Name "extends")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interfaceModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceModifierPublic :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceModifierProtected :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierProtected =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceModifierPrivate :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceModifierAbstract :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierAbstract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceModifierStatic :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceModifierStrictfb :: Phantoms.TTerm Syntax.InterfaceModifier
interfaceModifierStrictfb =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfb"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceBody :: Phantoms.TTerm [Syntax.InterfaceMemberDeclaration] -> Phantoms.TTerm Syntax.InterfaceBody
interfaceBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unInterfaceBody :: Phantoms.TTerm Syntax.InterfaceBody -> Phantoms.TTerm [Syntax.InterfaceMemberDeclaration]
unInterfaceBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.InterfaceBody")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceMemberDeclarationConstant :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationConstant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceMemberDeclarationInterfaceMethod :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterfaceMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interfaceMethod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceMemberDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceMemberDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.InterfaceMemberDeclaration
interfaceMemberDeclarationInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constantDeclaration :: Phantoms.TTerm [Syntax.ConstantModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclaration modifiers type_ variables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
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

constantDeclarationModifiers :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.ConstantModifier]
constantDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantDeclarationType :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.UnannType
constantDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantDeclarationVariables :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator]
constantDeclarationVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
        Core.projectionField = (Core.Name "variables")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantDeclarationWithModifiers :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.ConstantModifier] -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "variables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constantDeclarationWithType :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "variables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constantDeclarationWithVariables :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.ConstantDeclaration
constantDeclarationWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constantModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ConstantModifier
constantModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constantModifierPublic :: Phantoms.TTerm Syntax.ConstantModifier
constantModifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

constantModifierStatic :: Phantoms.TTerm Syntax.ConstantModifier
constantModifierStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

constantModifierFinal :: Phantoms.TTerm Syntax.ConstantModifier
constantModifierFinal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConstantModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceMethodDeclaration :: Phantoms.TTerm [Syntax.InterfaceMethodModifier] -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclaration modifiers header body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
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

interfaceMethodDeclarationModifiers :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm [Syntax.InterfaceMethodModifier]
interfaceMethodDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceMethodDeclarationHeader :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader
interfaceMethodDeclarationHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionField = (Core.Name "header")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceMethodDeclarationBody :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodBody
interfaceMethodDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interfaceMethodDeclarationWithModifiers :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm [Syntax.InterfaceMethodModifier] -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "header")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceMethodDeclarationWithHeader :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodHeader -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interfaceMethodDeclarationWithBody :: Phantoms.TTerm Syntax.InterfaceMethodDeclaration -> Phantoms.TTerm Syntax.MethodBody -> Phantoms.TTerm Syntax.InterfaceMethodDeclaration
interfaceMethodDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodDeclaration"),
              Core.projectionField = (Core.Name "header")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interfaceMethodModifierAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

interfaceMethodModifierPublic :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceMethodModifierPrivate :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceMethodModifierAbstract :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierAbstract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceMethodModifierDefault :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierDefault =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceMethodModifierStatic :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

interfaceMethodModifierStrictfp :: Phantoms.TTerm Syntax.InterfaceMethodModifier
interfaceMethodModifierStrictfp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.InterfaceMethodModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictfp"),
        Core.fieldTerm = Core.TermUnit}}))

annotationTypeDeclaration :: Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.AnnotationTypeBody -> Phantoms.TTerm Syntax.AnnotationTypeDeclaration
annotationTypeDeclaration modifiers identifier body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
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

annotationTypeDeclarationModifiers :: Phantoms.TTerm Syntax.AnnotationTypeDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier]
annotationTypeDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeDeclarationIdentifier :: Phantoms.TTerm Syntax.AnnotationTypeDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier
annotationTypeDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeDeclarationBody :: Phantoms.TTerm Syntax.AnnotationTypeDeclaration -> Phantoms.TTerm Syntax.AnnotationTypeBody
annotationTypeDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeDeclarationWithModifiers :: Phantoms.TTerm Syntax.AnnotationTypeDeclaration -> Phantoms.TTerm [Syntax.InterfaceModifier] -> Phantoms.TTerm Syntax.AnnotationTypeDeclaration
annotationTypeDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationTypeDeclarationWithIdentifier :: Phantoms.TTerm Syntax.AnnotationTypeDeclaration -> Phantoms.TTerm Syntax.TypeIdentifier -> Phantoms.TTerm Syntax.AnnotationTypeDeclaration
annotationTypeDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationTypeDeclarationWithBody :: Phantoms.TTerm Syntax.AnnotationTypeDeclaration -> Phantoms.TTerm Syntax.AnnotationTypeBody -> Phantoms.TTerm Syntax.AnnotationTypeDeclaration
annotationTypeDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationTypeBody :: Phantoms.TTerm [[Syntax.AnnotationTypeMemberDeclaration]] -> Phantoms.TTerm Syntax.AnnotationTypeBody
annotationTypeBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAnnotationTypeBody :: Phantoms.TTerm Syntax.AnnotationTypeBody -> Phantoms.TTerm [[Syntax.AnnotationTypeMemberDeclaration]]
unAnnotationTypeBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.AnnotationTypeBody")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeMemberDeclarationAnnotationType :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm Syntax.AnnotationTypeMemberDeclaration
annotationTypeMemberDeclarationAnnotationType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationTypeMemberDeclarationConstant :: Phantoms.TTerm Syntax.ConstantDeclaration -> Phantoms.TTerm Syntax.AnnotationTypeMemberDeclaration
annotationTypeMemberDeclarationConstant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationTypeMemberDeclarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.AnnotationTypeMemberDeclaration
annotationTypeMemberDeclarationClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationTypeMemberDeclarationInterface :: Phantoms.TTerm Syntax.InterfaceDeclaration -> Phantoms.TTerm Syntax.AnnotationTypeMemberDeclaration
annotationTypeMemberDeclarationInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeMemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationTypeElementDeclaration :: Phantoms.TTerm [Syntax.AnnotationTypeElementModifier] -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration
annotationTypeElementDeclaration modifiers type_ identifier dims default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
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

annotationTypeElementDeclarationModifiers :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm [Syntax.AnnotationTypeElementModifier]
annotationTypeElementDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeElementDeclarationType :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm Syntax.UnannType
annotationTypeElementDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeElementDeclarationIdentifier :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm Syntax.Identifier
annotationTypeElementDeclarationIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeElementDeclarationDims :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm (Maybe Syntax.Dims)
annotationTypeElementDeclarationDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
        Core.projectionField = (Core.Name "dims")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeElementDeclarationDefault :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm (Maybe Syntax.DefaultValue)
annotationTypeElementDeclarationDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
        Core.projectionField = (Core.Name "default")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationTypeElementDeclarationWithModifiers :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm [Syntax.AnnotationTypeElementModifier] -> Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration
annotationTypeElementDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationTypeElementDeclarationWithType :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration
annotationTypeElementDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationTypeElementDeclarationWithIdentifier :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration
annotationTypeElementDeclarationWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationTypeElementDeclarationWithDims :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration
annotationTypeElementDeclarationWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationTypeElementDeclarationWithDefault :: Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration -> Phantoms.TTerm (Maybe Syntax.DefaultValue) -> Phantoms.TTerm Syntax.AnnotationTypeElementDeclaration
annotationTypeElementDeclarationWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementDeclaration"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationTypeElementModifierPublic :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.AnnotationTypeElementModifier
annotationTypeElementModifierPublic x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationTypeElementModifierAbstract :: Phantoms.TTerm Syntax.AnnotationTypeElementModifier
annotationTypeElementModifierAbstract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotationTypeElementModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))

defaultValue :: Phantoms.TTerm Syntax.ElementValue -> Phantoms.TTerm Syntax.DefaultValue
defaultValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.DefaultValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDefaultValue :: Phantoms.TTerm Syntax.DefaultValue -> Phantoms.TTerm Syntax.ElementValue
unDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.DefaultValue")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationNormal :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm Syntax.Annotation
annotationNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationMarker :: Phantoms.TTerm Syntax.MarkerAnnotation -> Phantoms.TTerm Syntax.Annotation
annotationMarker x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "marker"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationSingleElement :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm Syntax.Annotation
annotationSingleElement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Annotation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

normalAnnotation :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.ElementValuePair] -> Phantoms.TTerm Syntax.NormalAnnotation
normalAnnotation typeName pairs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Phantoms.unTTerm pairs)}]}))

normalAnnotationTypeName :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm Syntax.TypeName
normalAnnotationTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalAnnotation"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalAnnotationPairs :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm [Syntax.ElementValuePair]
normalAnnotationPairs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalAnnotation"),
        Core.projectionField = (Core.Name "pairs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalAnnotationWithTypeName :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.NormalAnnotation
normalAnnotationWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalAnnotation"),
              Core.projectionField = (Core.Name "pairs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalAnnotationWithPairs :: Phantoms.TTerm Syntax.NormalAnnotation -> Phantoms.TTerm [Syntax.ElementValuePair] -> Phantoms.TTerm Syntax.NormalAnnotation
normalAnnotationWithPairs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.NormalAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.NormalAnnotation"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elementValuePair :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ElementValue -> Phantoms.TTerm Syntax.ElementValuePair
elementValuePair key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

elementValuePairKey :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.Identifier
elementValuePairKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ElementValuePair"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementValuePairValue :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.ElementValue
elementValuePairValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ElementValuePair"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementValuePairWithKey :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ElementValuePair
elementValuePairWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ElementValuePair"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementValuePairWithValue :: Phantoms.TTerm Syntax.ElementValuePair -> Phantoms.TTerm Syntax.ElementValue -> Phantoms.TTerm Syntax.ElementValuePair
elementValuePairWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ElementValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ElementValuePair"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elementValueConditionalExpression :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.ElementValue
elementValueConditionalExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditionalExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elementValueElementValueArrayInitializer :: Phantoms.TTerm Syntax.ElementValueArrayInitializer -> Phantoms.TTerm Syntax.ElementValue
elementValueElementValueArrayInitializer x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elementValueArrayInitializer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elementValueAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.ElementValue
elementValueAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ElementValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elementValueArrayInitializer :: Phantoms.TTerm [Syntax.ElementValue] -> Phantoms.TTerm Syntax.ElementValueArrayInitializer
elementValueArrayInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ElementValueArrayInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unElementValueArrayInitializer :: Phantoms.TTerm Syntax.ElementValueArrayInitializer -> Phantoms.TTerm [Syntax.ElementValue]
unElementValueArrayInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ElementValueArrayInitializer")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

markerAnnotation :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.MarkerAnnotation
markerAnnotation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.MarkerAnnotation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMarkerAnnotation :: Phantoms.TTerm Syntax.MarkerAnnotation -> Phantoms.TTerm Syntax.TypeName
unMarkerAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.MarkerAnnotation")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleElementAnnotation :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.ElementValue) -> Phantoms.TTerm Syntax.SingleElementAnnotation
singleElementAnnotation name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

singleElementAnnotationName :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm Syntax.TypeName
singleElementAnnotationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleElementAnnotation"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleElementAnnotationValue :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm (Maybe Syntax.ElementValue)
singleElementAnnotationValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleElementAnnotation"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleElementAnnotationWithName :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.SingleElementAnnotation
singleElementAnnotationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleElementAnnotation"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

singleElementAnnotationWithValue :: Phantoms.TTerm Syntax.SingleElementAnnotation -> Phantoms.TTerm (Maybe Syntax.ElementValue) -> Phantoms.TTerm Syntax.SingleElementAnnotation
singleElementAnnotationWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SingleElementAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SingleElementAnnotation"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayInitializer :: Phantoms.TTerm [[Syntax.VariableInitializer]] -> Phantoms.TTerm Syntax.ArrayInitializer
arrayInitializer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ArrayInitializer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unArrayInitializer :: Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm [[Syntax.VariableInitializer]]
unArrayInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ArrayInitializer")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

block :: Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.Block
block x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.Block"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.BlockStatement]
unBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.Block")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockStatementLocalVariableDeclaration :: Phantoms.TTerm Syntax.LocalVariableDeclarationStatement -> Phantoms.TTerm Syntax.BlockStatement
blockStatementLocalVariableDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariableDeclaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

blockStatementClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.BlockStatement
blockStatementClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

blockStatementStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.BlockStatement
blockStatementStatement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.BlockStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localVariableDeclarationStatement :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.LocalVariableDeclarationStatement
localVariableDeclarationStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclarationStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLocalVariableDeclarationStatement :: Phantoms.TTerm Syntax.LocalVariableDeclarationStatement -> Phantoms.TTerm Syntax.LocalVariableDeclaration
unLocalVariableDeclarationStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.LocalVariableDeclarationStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

localVariableDeclaration :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclaration modifiers type_ declarators =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
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

localVariableDeclarationModifiers :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableModifier]
localVariableDeclarationModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

localVariableDeclarationType :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.LocalVariableType
localVariableDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

localVariableDeclarationDeclarators :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator]
localVariableDeclarationDeclarators x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
        Core.projectionField = (Core.Name "declarators")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

localVariableDeclarationWithModifiers :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclarationWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "declarators")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

localVariableDeclarationWithType :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "declarators")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

localVariableDeclarationWithDeclarators :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.LocalVariableDeclaration
localVariableDeclarationWithDeclarators original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarators"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

localVariableTypeType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.LocalVariableType
localVariableTypeType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localVariableTypeVar :: Phantoms.TTerm Syntax.LocalVariableType
localVariableTypeVar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LocalVariableType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))

statementWithoutTrailing :: Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement -> Phantoms.TTerm Syntax.Statement
statementWithoutTrailing x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementLabeled :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
statementLabeled x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementIfThen :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Statement
statementIfThen x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThen"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementIfThenElse :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.Statement
statementIfThenElse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWhile :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement
statementWhile x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementFor :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Statement
statementFor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementNoShortIfWithoutTrailing :: Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfWithoutTrailing x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withoutTrailing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementNoShortIfLabeled :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfLabeled x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementNoShortIfIfThenElse :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfIfThenElse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifThenElse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementNoShortIfWhile :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfWhile x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementNoShortIfFor :: Phantoms.TTerm Syntax.ForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
statementNoShortIfFor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementEmpty :: Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementEmpty =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))

statementWithoutTrailingSubstatementExpression :: Phantoms.TTerm Syntax.ExpressionStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementAssert :: Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementAssert x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assert"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementSwitch :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSwitch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementDo :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementDo x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementBreak :: Phantoms.TTerm Syntax.BreakStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementBreak x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementContinue :: Phantoms.TTerm Syntax.ContinueStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementContinue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementReturn :: Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementReturn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementSynchronized :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementSynchronized x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "synchronized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementThrow :: Phantoms.TTerm Syntax.ThrowStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementThrow x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithoutTrailingSubstatementTry :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm Syntax.StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatementTry x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementWithoutTrailingSubstatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

labeledStatement :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatement identifier statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))

labeledStatementIdentifier :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Identifier
labeledStatementIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementStatement :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
labeledStatementStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "statement")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementWithIdentifier :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "statement")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

labeledStatementWithStatement :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

labeledStatementNoShortIf :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.LabeledStatementNoShortIf
labeledStatementNoShortIf identifier statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))

labeledStatementNoShortIfIdentifier :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.Identifier
labeledStatementNoShortIfIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementNoShortIfStatement :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
labeledStatementNoShortIfStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatementNoShortIf"),
        Core.projectionField = (Core.Name "statement")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementNoShortIfWithIdentifier :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatementNoShortIf"),
              Core.projectionField = (Core.Name "statement")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

labeledStatementNoShortIfWithStatement :: Phantoms.TTerm Syntax.LabeledStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.LabeledStatementNoShortIf
labeledStatementNoShortIfWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LabeledStatementNoShortIf"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

expressionStatement :: Phantoms.TTerm Syntax.StatementExpression -> Phantoms.TTerm Syntax.ExpressionStatement
expressionStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ExpressionStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unExpressionStatement :: Phantoms.TTerm Syntax.ExpressionStatement -> Phantoms.TTerm Syntax.StatementExpression
unExpressionStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ExpressionStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statementExpressionAssignment :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionAssignment x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementExpressionPreIncrement :: Phantoms.TTerm Syntax.PreIncrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPreIncrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementExpressionPreDecrement :: Phantoms.TTerm Syntax.PreDecrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPreDecrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementExpressionPostIncrement :: Phantoms.TTerm Syntax.PostIncrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPostIncrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementExpressionPostDecrement :: Phantoms.TTerm Syntax.PostDecrementExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionPostDecrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementExpressionMethodInvocation :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionMethodInvocation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementExpressionClassInstanceCreation :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.StatementExpression
statementExpressionClassInstanceCreation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.StatementExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstanceCreation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ifThenStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenStatement
ifThenStatement expression statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))

ifThenStatementExpression :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Expression
ifThenStatementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenStatement"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenStatementStatement :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Statement
ifThenStatementStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenStatement"),
        Core.projectionField = (Core.Name "statement")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenStatementWithExpression :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfThenStatement
ifThenStatementWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenStatement"),
              Core.projectionField = (Core.Name "statement")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifThenStatementWithStatement :: Phantoms.TTerm Syntax.IfThenStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenStatement
ifThenStatementWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenStatement"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifThenElseStatement :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatement cond then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
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

ifThenElseStatementCond :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
ifThenElseStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenElseStatementThen :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.StatementNoShortIf
ifThenElseStatementThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
        Core.projectionField = (Core.Name "then")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenElseStatementElse :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.Statement
ifThenElseStatementElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
        Core.projectionField = (Core.Name "else")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenElseStatementWithCond :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifThenElseStatementWithThen :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatementWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifThenElseStatementWithElse :: Phantoms.TTerm Syntax.IfThenElseStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfThenElseStatement
ifThenElseStatementWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatement"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifThenElseStatementNoShortIf :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIf cond then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
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

ifThenElseStatementNoShortIfCond :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression)
ifThenElseStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenElseStatementNoShortIfThen :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
ifThenElseStatementNoShortIfThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionField = (Core.Name "then")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenElseStatementNoShortIfElse :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
ifThenElseStatementNoShortIfElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
        Core.projectionField = (Core.Name "else")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThenElseStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifThenElseStatementNoShortIfWithThen :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifThenElseStatementNoShortIfWithElse :: Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIfWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.IfThenElseStatementNoShortIf"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

assertStatementSingle :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement
assertStatementSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertStatementPair :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.AssertStatement
assertStatementPair x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertStatement_Pair :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement_Pair
assertStatement_Pair first second =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm second)}]}))

assertStatement_PairFirst :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression
assertStatement_PairFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement_Pair"),
        Core.projectionField = (Core.Name "first")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assertStatement_PairSecond :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression
assertStatement_PairSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement_Pair"),
        Core.projectionField = (Core.Name "second")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assertStatement_PairWithFirst :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement_Pair
assertStatement_PairWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement_Pair"),
              Core.projectionField = (Core.Name "second")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assertStatement_PairWithSecond :: Phantoms.TTerm Syntax.AssertStatement_Pair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement_Pair
assertStatement_PairWithSecond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AssertStatement_Pair"),
              Core.projectionField = (Core.Name "first")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchBlock -> Phantoms.TTerm Syntax.SwitchStatement
switchStatement cond block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))

switchStatementCond :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression
switchStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchStatementBlock :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.SwitchBlock
switchStatementBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "block")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchStatementWithCond :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

switchStatementWithBlock :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.SwitchBlock -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchBlock :: Phantoms.TTerm [Syntax.SwitchBlock_Pair] -> Phantoms.TTerm Syntax.SwitchBlock
switchBlock x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSwitchBlock :: Phantoms.TTerm Syntax.SwitchBlock -> Phantoms.TTerm [Syntax.SwitchBlock_Pair]
unSwitchBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.SwitchBlock")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchBlock_Pair :: Phantoms.TTerm [Syntax.SwitchBlockStatementGroup] -> Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm Syntax.SwitchBlock_Pair
switchBlock_Pair statements labels =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm labels)}]}))

switchBlock_PairStatements :: Phantoms.TTerm Syntax.SwitchBlock_Pair -> Phantoms.TTerm [Syntax.SwitchBlockStatementGroup]
switchBlock_PairStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock_Pair"),
        Core.projectionField = (Core.Name "statements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchBlock_PairLabels :: Phantoms.TTerm Syntax.SwitchBlock_Pair -> Phantoms.TTerm [Syntax.SwitchLabel]
switchBlock_PairLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock_Pair"),
        Core.projectionField = (Core.Name "labels")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchBlock_PairWithStatements :: Phantoms.TTerm Syntax.SwitchBlock_Pair -> Phantoms.TTerm [Syntax.SwitchBlockStatementGroup] -> Phantoms.TTerm Syntax.SwitchBlock_Pair
switchBlock_PairWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock_Pair"),
              Core.projectionField = (Core.Name "labels")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

switchBlock_PairWithLabels :: Phantoms.TTerm Syntax.SwitchBlock_Pair -> Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm Syntax.SwitchBlock_Pair
switchBlock_PairWithLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock_Pair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlock_Pair"),
              Core.projectionField = (Core.Name "statements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchBlockStatementGroup :: Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.SwitchBlockStatementGroup
switchBlockStatementGroup labels statements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm labels)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)}]}))

switchBlockStatementGroupLabels :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.SwitchLabel]
switchBlockStatementGroupLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionField = (Core.Name "labels")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchBlockStatementGroupStatements :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.BlockStatement]
switchBlockStatementGroupStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlockStatementGroup"),
        Core.projectionField = (Core.Name "statements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchBlockStatementGroupWithLabels :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.SwitchLabel] -> Phantoms.TTerm Syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlockStatementGroup"),
              Core.projectionField = (Core.Name "statements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

switchBlockStatementGroupWithStatements :: Phantoms.TTerm Syntax.SwitchBlockStatementGroup -> Phantoms.TTerm [Syntax.BlockStatement] -> Phantoms.TTerm Syntax.SwitchBlockStatementGroup
switchBlockStatementGroupWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlockStatementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchBlockStatementGroup"),
              Core.projectionField = (Core.Name "labels")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchLabelConstant :: Phantoms.TTerm Syntax.ConstantExpression -> Phantoms.TTerm Syntax.SwitchLabel
switchLabelConstant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

switchLabelEnumConstant :: Phantoms.TTerm Syntax.EnumConstantName -> Phantoms.TTerm Syntax.SwitchLabel
switchLabelEnumConstant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumConstant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

switchLabelDefault :: Phantoms.TTerm Syntax.SwitchLabel
switchLabelDefault =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.SwitchLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))

enumConstantName :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.EnumConstantName
enumConstantName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.EnumConstantName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unEnumConstantName :: Phantoms.TTerm Syntax.EnumConstantName -> Phantoms.TTerm Syntax.Identifier
unEnumConstantName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.EnumConstantName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatement :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatement cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

whileStatementCond :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
whileStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement
whileStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementWithCond :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

whileStatementWithBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

whileStatementNoShortIf :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.WhileStatementNoShortIf
whileStatementNoShortIf cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

whileStatementNoShortIfCond :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression)
whileStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementNoShortIfBody :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
whileStatementNoShortIfBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatementNoShortIf"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatementNoShortIf"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

whileStatementNoShortIfWithBody :: Phantoms.TTerm Syntax.WhileStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.WhileStatementNoShortIf
whileStatementNoShortIfWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.WhileStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

doStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DoStatement
doStatement body conde =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "conde"),
          Core.fieldTerm = (Phantoms.unTTerm conde)}]}))

doStatementBody :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Statement
doStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DoStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doStatementConde :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
doStatementConde x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DoStatement"),
        Core.projectionField = (Core.Name "conde")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doStatementWithBody :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.DoStatement
doStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "conde"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DoStatement"),
              Core.projectionField = (Core.Name "conde")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

doStatementWithConde :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DoStatement
doStatementWithConde original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DoStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conde"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forStatementBasic :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.ForStatement
forStatementBasic x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forStatementEnhanced :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.ForStatement
forStatementEnhanced x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ForStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forStatementNoShortIfBasic :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.ForStatementNoShortIf
forStatementNoShortIfBasic x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forStatementNoShortIfEnhanced :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.ForStatementNoShortIf
forStatementNoShortIfEnhanced x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ForStatementNoShortIf"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enhanced"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

basicForStatement :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.BasicForStatement
basicForStatement cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

basicForStatementCond :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.ForCond
basicForStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatement"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

basicForStatementBody :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.Statement
basicForStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

basicForStatementWithCond :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.BasicForStatement
basicForStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

basicForStatementWithBody :: Phantoms.TTerm Syntax.BasicForStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.BasicForStatement
basicForStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatement"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forCond :: Phantoms.TTerm (Maybe Syntax.ForInit) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.ForUpdate) -> Phantoms.TTerm Syntax.ForCond
forCond init cond update =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
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

forCondInit :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForInit)
forCondInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forCondCond :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.Expression)
forCondCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forCondUpdate :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForUpdate)
forCondUpdate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
        Core.projectionField = (Core.Name "update")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forCondWithInit :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForInit) -> Phantoms.TTerm Syntax.ForCond
forCondWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "update")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forCondWithCond :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ForCond
forCondWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "update")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forCondWithUpdate :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm (Maybe Syntax.ForUpdate) -> Phantoms.TTerm Syntax.ForCond
forCondWithUpdate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ForCond"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

basicForStatementNoShortIf :: Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.BasicForStatementNoShortIf
basicForStatementNoShortIf cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

basicForStatementNoShortIfCond :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.ForCond
basicForStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

basicForStatementNoShortIfBody :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
basicForStatementNoShortIfBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatementNoShortIf"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

basicForStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.ForCond -> Phantoms.TTerm Syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatementNoShortIf"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

basicForStatementNoShortIfWithBody :: Phantoms.TTerm Syntax.BasicForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.BasicForStatementNoShortIf
basicForStatementNoShortIfWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.BasicForStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forInitStatements :: Phantoms.TTerm [Syntax.StatementExpression] -> Phantoms.TTerm Syntax.ForInit
forInitStatements x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statements"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forInitLocalVariable :: Phantoms.TTerm Syntax.LocalVariableDeclaration -> Phantoms.TTerm Syntax.ForInit
forInitLocalVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forUpdate :: Phantoms.TTerm [Syntax.StatementExpression] -> Phantoms.TTerm Syntax.ForUpdate
forUpdate x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ForUpdate"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unForUpdate :: Phantoms.TTerm Syntax.ForUpdate -> Phantoms.TTerm [Syntax.StatementExpression]
unForUpdate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ForUpdate")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForStatement :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.EnhancedForStatement
enhancedForStatement cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

enhancedForStatementCond :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForStatementCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatement"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForStatementBody :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.Statement
enhancedForStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForStatementWithCond :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.EnhancedForStatement
enhancedForStatementWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enhancedForStatementWithBody :: Phantoms.TTerm Syntax.EnhancedForStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.EnhancedForStatement
enhancedForStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatement"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enhancedForCond :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCond modifiers type_ id expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm modifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

enhancedForCondModifiers :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm [Syntax.VariableModifier]
enhancedForCondModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForCondType :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.LocalVariableType
enhancedForCondType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForCondId :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.VariableDeclaratorId
enhancedForCondId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForCondExpression :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.Expression
enhancedForCondExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForCondWithModifiers :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCondWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enhancedForCondWithType :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCondWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enhancedForCondWithId :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCondWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enhancedForCondWithExpression :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForCondWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForCond"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enhancedForStatementNoShortIf :: Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIf cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

enhancedForStatementNoShortIfCond :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForCond
enhancedForStatementNoShortIfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForStatementNoShortIfBody :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf
enhancedForStatementNoShortIfBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatementNoShortIf"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enhancedForStatementNoShortIfWithCond :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForCond -> Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatementNoShortIf"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enhancedForStatementNoShortIfWithBody :: Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf -> Phantoms.TTerm Syntax.StatementNoShortIf -> Phantoms.TTerm Syntax.EnhancedForStatementNoShortIf
enhancedForStatementNoShortIfWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatementNoShortIf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EnhancedForStatementNoShortIf"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

breakStatement :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.BreakStatement
breakStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.BreakStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unBreakStatement :: Phantoms.TTerm Syntax.BreakStatement -> Phantoms.TTerm (Maybe Syntax.Identifier)
unBreakStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.BreakStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

continueStatement :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ContinueStatement
continueStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ContinueStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unContinueStatement :: Phantoms.TTerm Syntax.ContinueStatement -> Phantoms.TTerm (Maybe Syntax.Identifier)
unContinueStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ContinueStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

returnStatement :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ReturnStatement
returnStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ReturnStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unReturnStatement :: Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
unReturnStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ReturnStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

throwStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ThrowStatement
throwStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ThrowStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unThrowStatement :: Phantoms.TTerm Syntax.ThrowStatement -> Phantoms.TTerm Syntax.Expression
unThrowStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ThrowStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

synchronizedStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.SynchronizedStatement
synchronizedStatement expression block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))

synchronizedStatementExpression :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Expression
synchronizedStatementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SynchronizedStatement"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

synchronizedStatementBlock :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Block
synchronizedStatementBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SynchronizedStatement"),
        Core.projectionField = (Core.Name "block")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

synchronizedStatementWithExpression :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SynchronizedStatement
synchronizedStatementWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SynchronizedStatement"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

synchronizedStatementWithBlock :: Phantoms.TTerm Syntax.SynchronizedStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.SynchronizedStatement
synchronizedStatementWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.SynchronizedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.SynchronizedStatement"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tryStatementSimple :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.TryStatement
tryStatementSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tryStatementWithFinally :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.TryStatement
tryStatementWithFinally x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withFinally"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tryStatementWithResources :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.TryStatement
tryStatementWithResources x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "withResources"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tryStatement_Simple :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Catches -> Phantoms.TTerm Syntax.TryStatement_Simple
tryStatement_Simple block catches =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm catches)}]}))

tryStatement_SimpleBlock :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Block
tryStatement_SimpleBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_Simple"),
        Core.projectionField = (Core.Name "block")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatement_SimpleCatches :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Catches
tryStatement_SimpleCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_Simple"),
        Core.projectionField = (Core.Name "catches")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatement_SimpleWithBlock :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryStatement_Simple
tryStatement_SimpleWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_Simple"),
              Core.projectionField = (Core.Name "catches")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryStatement_SimpleWithCatches :: Phantoms.TTerm Syntax.TryStatement_Simple -> Phantoms.TTerm Syntax.Catches -> Phantoms.TTerm Syntax.TryStatement_Simple
tryStatement_SimpleWithCatches original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_Simple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_Simple"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tryStatement_WithFinally :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm Syntax.Finally -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinally block catches finally =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
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

tryStatement_WithFinallyBlock :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Block
tryStatement_WithFinallyBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
        Core.projectionField = (Core.Name "block")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatement_WithFinallyCatches :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm (Maybe Syntax.Catches)
tryStatement_WithFinallyCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
        Core.projectionField = (Core.Name "catches")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatement_WithFinallyFinally :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Finally
tryStatement_WithFinallyFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
        Core.projectionField = (Core.Name "finally")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatement_WithFinallyWithBlock :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "catches")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "finally")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryStatement_WithFinallyWithCatches :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithCatches original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "finally")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryStatement_WithFinallyWithFinally :: Phantoms.TTerm Syntax.TryStatement_WithFinally -> Phantoms.TTerm Syntax.Finally -> Phantoms.TTerm Syntax.TryStatement_WithFinally
tryStatement_WithFinallyWithFinally original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryStatement_WithFinally"),
              Core.projectionField = (Core.Name "catches")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

catches :: Phantoms.TTerm [Syntax.CatchClause] -> Phantoms.TTerm Syntax.Catches
catches x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.Catches"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unCatches :: Phantoms.TTerm Syntax.Catches -> Phantoms.TTerm [Syntax.CatchClause]
unCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.Catches")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchClause :: Phantoms.TTerm (Maybe Syntax.CatchFormalParameter) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CatchClause
catchClause parameter block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))

catchClauseParameter :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm (Maybe Syntax.CatchFormalParameter)
catchClauseParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchClause"),
        Core.projectionField = (Core.Name "parameter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchClauseBlock :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm Syntax.Block
catchClauseBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchClause"),
        Core.projectionField = (Core.Name "block")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchClauseWithParameter :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm (Maybe Syntax.CatchFormalParameter) -> Phantoms.TTerm Syntax.CatchClause
catchClauseWithParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchClause"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

catchClauseWithBlock :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CatchClause
catchClauseWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchClause"),
              Core.projectionField = (Core.Name "parameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

catchFormalParameter :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameter modifiers type_ id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
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

catchFormalParameterModifiers :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm [Syntax.VariableModifier]
catchFormalParameterModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchFormalParameterType :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.CatchType
catchFormalParameterType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchFormalParameterId :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.VariableDeclaratorId
catchFormalParameterId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchFormalParameterWithModifiers :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameterWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

catchFormalParameterWithType :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameterWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

catchFormalParameterWithId :: Phantoms.TTerm Syntax.CatchFormalParameter -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.CatchFormalParameter
catchFormalParameterWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchFormalParameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

catchType :: Phantoms.TTerm Syntax.UnannClassType -> Phantoms.TTerm [Syntax.ClassType] -> Phantoms.TTerm Syntax.CatchType
catchType type_ types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))

catchTypeType :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.UnannClassType
catchTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchType"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchTypeTypes :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm [Syntax.ClassType]
catchTypeTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchType"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchTypeWithType :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm Syntax.UnannClassType -> Phantoms.TTerm Syntax.CatchType
catchTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchType"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

catchTypeWithTypes :: Phantoms.TTerm Syntax.CatchType -> Phantoms.TTerm [Syntax.ClassType] -> Phantoms.TTerm Syntax.CatchType
catchTypeWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CatchType"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

finally :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Finally
finally x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.Finally"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFinally :: Phantoms.TTerm Syntax.Finally -> Phantoms.TTerm Syntax.Block
unFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.Finally")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithResourcesStatement :: Phantoms.TTerm Syntax.ResourceSpecification -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm (Maybe Syntax.Finally) -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatement resourceSpecification block catches finally =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
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

tryWithResourcesStatementResourceSpecification :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.ResourceSpecification
tryWithResourcesStatementResourceSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "resourceSpecification")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithResourcesStatementBlock :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.Block
tryWithResourcesStatementBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "block")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithResourcesStatementCatches :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Catches)
tryWithResourcesStatementCatches x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "catches")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithResourcesStatementFinally :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Finally)
tryWithResourcesStatementFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
        Core.projectionField = (Core.Name "finally")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithResourcesStatementWithResourceSpecification :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.ResourceSpecification -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithResourceSpecification original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "catches")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "finally")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryWithResourcesStatementWithBlock :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "resourceSpecification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "catches")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "finally")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryWithResourcesStatementWithCatches :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Catches) -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithCatches original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "resourceSpecification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "finally")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryWithResourcesStatementWithFinally :: Phantoms.TTerm Syntax.TryWithResourcesStatement -> Phantoms.TTerm (Maybe Syntax.Finally) -> Phantoms.TTerm Syntax.TryWithResourcesStatement
tryWithResourcesStatementWithFinally original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "resourceSpecification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "resourceSpecification")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catches"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.TryWithResourcesStatement"),
              Core.projectionField = (Core.Name "catches")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

resourceSpecification :: Phantoms.TTerm [Syntax.Resource] -> Phantoms.TTerm Syntax.ResourceSpecification
resourceSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ResourceSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unResourceSpecification :: Phantoms.TTerm Syntax.ResourceSpecification -> Phantoms.TTerm [Syntax.Resource]
unResourceSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ResourceSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

resourceLocal :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Resource
resourceLocal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resourceVariable :: Phantoms.TTerm Syntax.VariableAccess -> Phantoms.TTerm Syntax.Resource
resourceVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resource_Local :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Resource_Local
resource_Local modifiers type_ identifier expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
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

resource_LocalModifiers :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm [Syntax.VariableModifier]
resource_LocalModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

resource_LocalType :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.LocalVariableType
resource_LocalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

resource_LocalIdentifier :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Identifier
resource_LocalIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

resource_LocalExpression :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Expression
resource_LocalExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

resource_LocalWithModifiers :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

resource_LocalWithType :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.LocalVariableType -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

resource_LocalWithIdentifier :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

resource_LocalWithExpression :: Phantoms.TTerm Syntax.Resource_Local -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Resource_Local
resource_LocalWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Resource_Local"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableAccessExpressionName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.VariableAccess
variableAccessExpressionName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

variableAccessFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.VariableAccess
variableAccessFieldAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.VariableAccess"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArray :: Phantoms.TTerm Syntax.PrimaryNoNewArray -> Phantoms.TTerm Syntax.Primary
primaryNoNewArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noNewArray"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryArrayCreation :: Phantoms.TTerm Syntax.ArrayCreationExpression -> Phantoms.TTerm Syntax.Primary
primaryArrayCreation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayCreation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayClassLiteral :: Phantoms.TTerm Syntax.ClassLiteral -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayClassLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classLiteral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayThis :: Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayThis =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))

primaryNoNewArrayDotThis :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayDotThis x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dotThis"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayParens :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayClassInstance :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayClassInstance x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classInstance"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayFieldAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayArrayAccess :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayArrayAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayMethodInvocation :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayMethodInvocation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodInvocation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryNoNewArrayMethodReference :: Phantoms.TTerm Syntax.MethodReference -> Phantoms.TTerm Syntax.PrimaryNoNewArray
primaryNoNewArrayMethodReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PrimaryNoNewArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodReference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classLiteralType :: Phantoms.TTerm Syntax.TypeNameArray -> Phantoms.TTerm Syntax.ClassLiteral
classLiteralType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classLiteralNumericType :: Phantoms.TTerm Syntax.NumericTypeArray -> Phantoms.TTerm Syntax.ClassLiteral
classLiteralNumericType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numericType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classLiteralBoolean :: Phantoms.TTerm Syntax.BooleanArray -> Phantoms.TTerm Syntax.ClassLiteral
classLiteralBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classLiteralVoid :: Phantoms.TTerm Syntax.ClassLiteral
classLiteralVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))

typeNameArraySimple :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.TypeNameArray
typeNameArraySimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeNameArrayArray :: Phantoms.TTerm Syntax.TypeNameArray -> Phantoms.TTerm Syntax.TypeNameArray
typeNameArrayArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeNameArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericTypeArraySimple :: Phantoms.TTerm Syntax.NumericType -> Phantoms.TTerm Syntax.NumericTypeArray
numericTypeArraySimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericTypeArrayArray :: Phantoms.TTerm Syntax.NumericTypeArray -> Phantoms.TTerm Syntax.NumericTypeArray
numericTypeArrayArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.NumericTypeArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanArraySimple :: Phantoms.TTerm Syntax.BooleanArray
booleanArraySimple =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))

booleanArrayArray :: Phantoms.TTerm Syntax.BooleanArray -> Phantoms.TTerm Syntax.BooleanArray
booleanArrayArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.BooleanArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classInstanceCreationExpression :: Phantoms.TTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier) -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression
classInstanceCreationExpression qualifier expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

classInstanceCreationExpressionQualifier :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier)
classInstanceCreationExpressionQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "qualifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classInstanceCreationExpressionExpression :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
classInstanceCreationExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classInstanceCreationExpressionWithQualifier :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassInstanceCreationExpression_Qualifier) -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classInstanceCreationExpressionWithExpression :: Phantoms.TTerm Syntax.ClassInstanceCreationExpression -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression
classInstanceCreationExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classInstanceCreationExpression_QualifierExpression :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classInstanceCreationExpression_QualifierPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.ClassInstanceCreationExpression_Qualifier
classInstanceCreationExpression_QualifierPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unqualifiedClassInstanceCreationExpression :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression typeArguments classOrInterface arguments body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
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

unqualifiedClassInstanceCreationExpressionTypeArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.TypeArgument]
unqualifiedClassInstanceCreationExpressionTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unqualifiedClassInstanceCreationExpressionClassOrInterface :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
unqualifiedClassInstanceCreationExpressionClassOrInterface x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "classOrInterface")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unqualifiedClassInstanceCreationExpressionArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.Expression]
unqualifiedClassInstanceCreationExpressionArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unqualifiedClassInstanceCreationExpressionBody :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassBody)
unqualifiedClassInstanceCreationExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unqualifiedClassInstanceCreationExpressionWithTypeArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "classOrInterface")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unqualifiedClassInstanceCreationExpressionWithClassOrInterface :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithClassOrInterface original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unqualifiedClassInstanceCreationExpressionWithArguments :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "classOrInterface")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unqualifiedClassInstanceCreationExpressionWithBody :: Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classOrInterface"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "classOrInterface")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classOrInterfaceTypeToInstantiate :: Phantoms.TTerm [Syntax.AnnotatedIdentifier] -> Phantoms.TTerm (Maybe Syntax.TypeArgumentsOrDiamond) -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate identifiers typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm identifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))

classOrInterfaceTypeToInstantiateIdentifiers :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm [Syntax.AnnotatedIdentifier]
classOrInterfaceTypeToInstantiateIdentifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionField = (Core.Name "identifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classOrInterfaceTypeToInstantiateTypeArguments :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm (Maybe Syntax.TypeArgumentsOrDiamond)
classOrInterfaceTypeToInstantiateTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classOrInterfaceTypeToInstantiateWithIdentifiers :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm [Syntax.AnnotatedIdentifier] -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithIdentifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classOrInterfaceTypeToInstantiateWithTypeArguments :: Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate -> Phantoms.TTerm (Maybe Syntax.TypeArgumentsOrDiamond) -> Phantoms.TTerm Syntax.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiateWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate"),
              Core.projectionField = (Core.Name "identifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotatedIdentifier :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AnnotatedIdentifier
annotatedIdentifier annotations identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))

annotatedIdentifierAnnotations :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm [Syntax.Annotation]
annotatedIdentifierAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotatedIdentifier"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedIdentifierIdentifier :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm Syntax.Identifier
annotatedIdentifierIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotatedIdentifier"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedIdentifierWithAnnotations :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotatedIdentifier
annotatedIdentifierWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotatedIdentifier"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotatedIdentifierWithIdentifier :: Phantoms.TTerm Syntax.AnnotatedIdentifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AnnotatedIdentifier
annotatedIdentifierWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AnnotatedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AnnotatedIdentifier"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeArgumentsOrDiamondArguments :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondArguments x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arguments"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeArgumentsOrDiamondDiamond :: Phantoms.TTerm Syntax.TypeArgumentsOrDiamond
typeArgumentsOrDiamondDiamond =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.TypeArgumentsOrDiamond"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "diamond"),
        Core.fieldTerm = Core.TermUnit}}))

fieldAccess :: Phantoms.TTerm Syntax.FieldAccess_Qualifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.FieldAccess
fieldAccess qualifier identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))

fieldAccessQualifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccessQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess"),
        Core.projectionField = (Core.Name "qualifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAccessIdentifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Identifier
fieldAccessIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAccessWithQualifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.FieldAccess_Qualifier -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldAccessWithIdentifier :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldAccess_QualifierPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fieldAccess_QualifierSuper :: Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierSuper =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))

fieldAccess_QualifierTyped :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.FieldAccess_Qualifier
fieldAccess_QualifierTyped x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.FieldAccess_Qualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayAccess :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ArrayAccess_Variant -> Phantoms.TTerm Syntax.ArrayAccess
arrayAccess expression variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))

arrayAccessExpression :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm (Maybe Syntax.Expression)
arrayAccessExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayAccessVariant :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.ArrayAccess_Variant
arrayAccessVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess"),
        Core.projectionField = (Core.Name "variant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayAccessWithExpression :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ArrayAccess
arrayAccessWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayAccessWithVariant :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.ArrayAccess_Variant -> Phantoms.TTerm Syntax.ArrayAccess
arrayAccessWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayAccess_VariantName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayAccess_VariantPrimary :: Phantoms.TTerm Syntax.PrimaryNoNewArray -> Phantoms.TTerm Syntax.ArrayAccess_Variant
arrayAccess_VariantPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayAccess_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodInvocation :: Phantoms.TTerm Syntax.MethodInvocation_Header -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MethodInvocation
methodInvocation header arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

methodInvocationHeader :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.MethodInvocation_Header
methodInvocationHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation"),
        Core.projectionField = (Core.Name "header")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodInvocationArguments :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm [Syntax.Expression]
methodInvocationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodInvocationWithHeader :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.MethodInvocation_Header -> Phantoms.TTerm Syntax.MethodInvocation
methodInvocationWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodInvocationWithArguments :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MethodInvocation
methodInvocationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation"),
              Core.projectionField = (Core.Name "header")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodInvocation_HeaderSimple :: Phantoms.TTerm Syntax.MethodName -> Phantoms.TTerm Syntax.MethodInvocation_Header
methodInvocation_HeaderSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodInvocation_HeaderComplex :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.MethodInvocation_Header
methodInvocation_HeaderComplex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Header"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodInvocation_Complex :: Phantoms.TTerm Syntax.MethodInvocation_Variant -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_Complex variant typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
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

methodInvocation_ComplexVariant :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_ComplexVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
        Core.projectionField = (Core.Name "variant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodInvocation_ComplexTypeArguments :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm [Syntax.TypeArgument]
methodInvocation_ComplexTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodInvocation_ComplexIdentifier :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.Identifier
methodInvocation_ComplexIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodInvocation_ComplexWithVariant :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.MethodInvocation_Variant -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_ComplexWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodInvocation_ComplexWithTypeArguments :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_ComplexWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodInvocation_ComplexWithIdentifier :: Phantoms.TTerm Syntax.MethodInvocation_Complex -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodInvocation_Complex
methodInvocation_ComplexWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Complex"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodInvocation_VariantType :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodInvocation_VariantExpression :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodInvocation_VariantPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodInvocation_VariantSuper :: Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantSuper =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))

methodInvocation_VariantTypeSuper :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.MethodInvocation_Variant
methodInvocation_VariantTypeSuper x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodInvocation_Variant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSuper"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodReferenceExpression :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.MethodReference
methodReferenceExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodReferencePrimary :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.MethodReference
methodReferencePrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodReferenceReferenceType :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.MethodReference
methodReferenceReferenceType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "referenceType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodReferenceSuper :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Syntax.MethodReference
methodReferenceSuper x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodReferenceNew :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm Syntax.MethodReference
methodReferenceNew x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodReferenceArray :: Phantoms.TTerm Syntax.MethodReference_Array -> Phantoms.TTerm Syntax.MethodReference
methodReferenceArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

methodReference_Expression :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_Expression name typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
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

methodReference_ExpressionName :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.ExpressionName
methodReference_ExpressionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_ExpressionTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_ExpressionTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_ExpressionIdentifier :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.Identifier
methodReference_ExpressionIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_ExpressionWithName :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_ExpressionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_ExpressionWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_ExpressionWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_ExpressionWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_Expression -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Expression
methodReference_ExpressionWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Expression"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodReference_Primary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_Primary primary typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
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

methodReference_PrimaryPrimary :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Primary
methodReference_PrimaryPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
        Core.projectionField = (Core.Name "primary")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_PrimaryTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_PrimaryTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_PrimaryIdentifier :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Identifier
methodReference_PrimaryIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_PrimaryWithPrimary :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_PrimaryWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_PrimaryWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_PrimaryWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "primary")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_PrimaryWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_Primary -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Primary
methodReference_PrimaryWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "primary")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Primary"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodReference_ReferenceType :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceType referenceType typeArguments identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
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

methodReference_ReferenceTypeReferenceType :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.ReferenceType
methodReference_ReferenceTypeReferenceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
        Core.projectionField = (Core.Name "referenceType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_ReferenceTypeTypeArguments :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_ReferenceTypeTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_ReferenceTypeIdentifier :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.Identifier
methodReference_ReferenceTypeIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_ReferenceTypeWithReferenceType :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithReferenceType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_ReferenceTypeWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "referenceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_ReferenceTypeWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_ReferenceType -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_ReferenceType
methodReference_ReferenceTypeWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "referenceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "referenceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_ReferenceType"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodReference_Super :: Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_Super typeArguments identifier super =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
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

methodReference_SuperTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_SuperTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_SuperIdentifier :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Syntax.Identifier
methodReference_SuperIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_SuperSuper :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Bool
methodReference_SuperSuper x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
        Core.projectionField = (Core.Name "super")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_SuperWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_SuperWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "super")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_SuperWithIdentifier :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_SuperWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "super")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_SuperWithSuper :: Phantoms.TTerm Syntax.MethodReference_Super -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MethodReference_Super
methodReference_SuperWithSuper original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Super"),
              Core.projectionField = (Core.Name "identifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "super"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodReference_New :: Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_New
methodReference_New classType typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Phantoms.unTTerm classType)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))

methodReference_NewClassType :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm Syntax.ClassType
methodReference_NewClassType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_New"),
        Core.projectionField = (Core.Name "classType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_NewTypeArguments :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm [Syntax.TypeArgument]
methodReference_NewTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_New"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodReference_NewWithClassType :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm Syntax.ClassType -> Phantoms.TTerm Syntax.MethodReference_New
methodReference_NewWithClassType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_New"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodReference_NewWithTypeArguments :: Phantoms.TTerm Syntax.MethodReference_New -> Phantoms.TTerm [Syntax.TypeArgument] -> Phantoms.TTerm Syntax.MethodReference_New
methodReference_NewWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_New"),
              Core.projectionField = (Core.Name "classType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodReference_Array :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.MethodReference_Array
methodReference_Array x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.MethodReference_Array"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMethodReference_Array :: Phantoms.TTerm Syntax.MethodReference_Array -> Phantoms.TTerm Syntax.ArrayType
unMethodReference_Array x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.MethodReference_Array")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpressionPrimitive :: Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive -> Phantoms.TTerm Syntax.ArrayCreationExpression
arrayCreationExpressionPrimitive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayCreationExpressionClassOrInterface :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface -> Phantoms.TTerm Syntax.ArrayCreationExpression
arrayCreationExpressionClassOrInterface x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayCreationExpressionPrimitiveArray :: Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray -> Phantoms.TTerm Syntax.ArrayCreationExpression
arrayCreationExpressionPrimitiveArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitiveArray"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayCreationExpressionClassOrInterfaceArray :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray -> Phantoms.TTerm Syntax.ArrayCreationExpression
arrayCreationExpressionClassOrInterfaceArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classOrInterfaceArray"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayCreationExpression_Primitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive
arrayCreationExpression_Primitive type_ dimExprs dims =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
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

arrayCreationExpression_PrimitiveType :: Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
arrayCreationExpression_PrimitiveType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_PrimitiveDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive -> Phantoms.TTerm [Syntax.DimExpr]
arrayCreationExpression_PrimitiveDimExprs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
        Core.projectionField = (Core.Name "dimExprs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_PrimitiveDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive -> Phantoms.TTerm (Maybe Syntax.Dims)
arrayCreationExpression_PrimitiveDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
        Core.projectionField = (Core.Name "dims")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_PrimitiveWithType :: Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive
arrayCreationExpression_PrimitiveWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
              Core.projectionField = (Core.Name "dimExprs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_PrimitiveWithDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive
arrayCreationExpression_PrimitiveWithDimExprs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_PrimitiveWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpression_Primitive
arrayCreationExpression_PrimitiveWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_Primitive"),
              Core.projectionField = (Core.Name "dimExprs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayCreationExpression_ClassOrInterface :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface
arrayCreationExpression_ClassOrInterface type_ dimExprs dims =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
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

arrayCreationExpression_ClassOrInterfaceType :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType
arrayCreationExpression_ClassOrInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_ClassOrInterfaceDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface -> Phantoms.TTerm [Syntax.DimExpr]
arrayCreationExpression_ClassOrInterfaceDimExprs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
        Core.projectionField = (Core.Name "dimExprs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_ClassOrInterfaceDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface -> Phantoms.TTerm (Maybe Syntax.Dims)
arrayCreationExpression_ClassOrInterfaceDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
        Core.projectionField = (Core.Name "dims")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_ClassOrInterfaceWithType :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface -> Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface
arrayCreationExpression_ClassOrInterfaceWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
              Core.projectionField = (Core.Name "dimExprs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_ClassOrInterfaceWithDimExprs :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface -> Phantoms.TTerm [Syntax.DimExpr] -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface
arrayCreationExpression_ClassOrInterfaceWithDimExprs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_ClassOrInterfaceWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface -> Phantoms.TTerm (Maybe Syntax.Dims) -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterface
arrayCreationExpression_ClassOrInterfaceWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dimExprs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface"),
              Core.projectionField = (Core.Name "dimExprs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayCreationExpression_PrimitiveArray :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray
arrayCreationExpression_PrimitiveArray type_ dims array =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
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

arrayCreationExpression_PrimitiveArrayType :: Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
arrayCreationExpression_PrimitiveArrayType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_PrimitiveArrayDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray -> Phantoms.TTerm [Syntax.Dims]
arrayCreationExpression_PrimitiveArrayDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
        Core.projectionField = (Core.Name "dims")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_PrimitiveArrayArray :: Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray -> Phantoms.TTerm Syntax.ArrayInitializer
arrayCreationExpression_PrimitiveArrayArray x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
        Core.projectionField = (Core.Name "array")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_PrimitiveArrayWithType :: Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray
arrayCreationExpression_PrimitiveArrayWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
              Core.projectionField = (Core.Name "array")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_PrimitiveArrayWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray
arrayCreationExpression_PrimitiveArrayWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
              Core.projectionField = (Core.Name "array")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_PrimitiveArrayWithArray :: Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpression_PrimitiveArray
arrayCreationExpression_PrimitiveArrayWithArray original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayCreationExpression_ClassOrInterfaceArray :: Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray
arrayCreationExpression_ClassOrInterfaceArray type_ dims array =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
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

arrayCreationExpression_ClassOrInterfaceArrayType :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray -> Phantoms.TTerm Syntax.ClassOrInterfaceType
arrayCreationExpression_ClassOrInterfaceArrayType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_ClassOrInterfaceArrayDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray -> Phantoms.TTerm [Syntax.Dims]
arrayCreationExpression_ClassOrInterfaceArrayDims x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
        Core.projectionField = (Core.Name "dims")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_ClassOrInterfaceArrayArray :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray -> Phantoms.TTerm Syntax.ArrayInitializer
arrayCreationExpression_ClassOrInterfaceArrayArray x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
        Core.projectionField = (Core.Name "array")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayCreationExpression_ClassOrInterfaceArrayWithType :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray -> Phantoms.TTerm Syntax.ClassOrInterfaceType -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray
arrayCreationExpression_ClassOrInterfaceArrayWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
              Core.projectionField = (Core.Name "array")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_ClassOrInterfaceArrayWithDims :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray -> Phantoms.TTerm [Syntax.Dims] -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray
arrayCreationExpression_ClassOrInterfaceArrayWithDims original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
              Core.projectionField = (Core.Name "array")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayCreationExpression_ClassOrInterfaceArrayWithArray :: Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray -> Phantoms.TTerm Syntax.ArrayInitializer -> Phantoms.TTerm Syntax.ArrayCreationExpression_ClassOrInterfaceArray
arrayCreationExpression_ClassOrInterfaceArrayWithArray original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dims"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray"),
              Core.projectionField = (Core.Name "dims")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dimExpr :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DimExpr
dimExpr annotations expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

dimExprAnnotations :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm [Syntax.Annotation]
dimExprAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DimExpr"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dimExprExpression :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm (Maybe Syntax.Expression)
dimExprExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DimExpr"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dimExprWithAnnotations :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DimExpr
dimExprWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DimExpr"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dimExprWithExpression :: Phantoms.TTerm Syntax.DimExpr -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DimExpr
dimExprWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.DimExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.DimExpr"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

expressionLambda :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionAssignment :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression
expressionAssignment x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lambdaExpression :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.LambdaBody -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpression parameters body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

lambdaExpressionParameters :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaParameters
lambdaExpressionParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionBody :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaBody
lambdaExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionWithParameters :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaExpressionWithBody :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.LambdaBody -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaParametersTuple :: Phantoms.TTerm [Syntax.LambdaParameters] -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lambdaParametersSingle :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lambdaParameterNormal :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.LambdaParameter
lambdaParameterNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lambdaParameterVariableArity :: Phantoms.TTerm Syntax.VariableArityParameter -> Phantoms.TTerm Syntax.LambdaParameter
lambdaParameterVariableArity x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableArity"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lambdaParameter_Normal :: Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LambdaParameterType -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_Normal modifiers type_ id =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
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

lambdaParameter_NormalModifiers :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm [Syntax.VariableModifier]
lambdaParameter_NormalModifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
        Core.projectionField = (Core.Name "modifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaParameter_NormalType :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.LambdaParameterType
lambdaParameter_NormalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaParameter_NormalId :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.VariableDeclaratorId
lambdaParameter_NormalId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaParameter_NormalWithModifiers :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm [Syntax.VariableModifier] -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_NormalWithModifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaParameter_NormalWithType :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.LambdaParameterType -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_NormalWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaParameter_NormalWithId :: Phantoms.TTerm Syntax.LambdaParameter_Normal -> Phantoms.TTerm Syntax.VariableDeclaratorId -> Phantoms.TTerm Syntax.LambdaParameter_Normal
lambdaParameter_NormalWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "modifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameter_Normal"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaParameterTypeType :: Phantoms.TTerm Syntax.UnannType -> Phantoms.TTerm Syntax.LambdaParameterType
lambdaParameterTypeType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lambdaParameterTypeVar :: Phantoms.TTerm Syntax.LambdaParameterType
lambdaParameterTypeVar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaParameterType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))

lambdaBodyExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaBody
lambdaBodyExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lambdaBodyBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.LambdaBody
lambdaBodyBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LambdaBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assignmentExpressionConditional :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionConditional x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assignmentExpressionAssignment :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionAssignment x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assignment :: Phantoms.TTerm Syntax.LeftHandSide -> Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Assignment
assignment lhs op expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
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

assignmentLhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.LeftHandSide
assignmentLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentOp :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignmentOperator
assignmentOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentExpression :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.Expression
assignmentExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentWithLhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.LeftHandSide -> Phantoms.TTerm Syntax.Assignment
assignmentWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignmentWithOp :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.Assignment
assignmentWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignmentWithExpression :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Assignment
assignmentWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.Assignment"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

leftHandSideExpressionName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.LeftHandSide
leftHandSideExpressionName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expressionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

leftHandSideFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.LeftHandSide
leftHandSideFieldAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

leftHandSideArrayAccess :: Phantoms.TTerm Syntax.ArrayAccess -> Phantoms.TTerm Syntax.LeftHandSide
leftHandSideArrayAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.LeftHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assignmentOperatorSimple :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorSimple =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorTimes :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorTimes =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorDiv :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorDiv =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorMod :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorMod =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorPlus :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorPlus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorMinus :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorMinus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorShiftLeft :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorShiftLeft =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorShiftRight :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorShiftRight =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorShiftRightZeroFill :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorShiftRightZeroFill =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorAnd :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorXor :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorXor =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorOr :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

conditionalExpressionSimple :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

conditionalExpressionTernaryCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionTernaryCond x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryCond"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

conditionalExpressionTernaryLambda :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionTernaryLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternaryLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

conditionalExpression_TernaryCond :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCond cond ifTrue ifFalse =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
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

conditionalExpression_TernaryCondCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalOrExpression
conditionalExpression_TernaryCondCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpression_TernaryCondIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.Expression
conditionalExpression_TernaryCondIfTrue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionField = (Core.Name "ifTrue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpression_TernaryCondIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpression_TernaryCondIfFalse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
        Core.projectionField = (Core.Name "ifFalse")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpression_TernaryCondWithCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifTrue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifFalse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

conditionalExpression_TernaryCondWithIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfTrue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifFalse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

conditionalExpression_TernaryCondWithIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond -> Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryCond
conditionalExpression_TernaryCondWithIfFalse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryCond"),
              Core.projectionField = (Core.Name "ifTrue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

conditionalExpression_TernaryLambda :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambda cond ifTrue ifFalse =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
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

conditionalExpression_TernaryLambdaCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.ConditionalOrExpression
conditionalExpression_TernaryLambdaCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpression_TernaryLambdaIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.Expression
conditionalExpression_TernaryLambdaIfTrue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionField = (Core.Name "ifTrue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpression_TernaryLambdaIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.LambdaExpression
conditionalExpression_TernaryLambdaIfFalse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
        Core.projectionField = (Core.Name "ifFalse")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpression_TernaryLambdaWithCond :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifTrue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifFalse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

conditionalExpression_TernaryLambdaWithIfTrue :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfTrue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifFalse")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

conditionalExpression_TernaryLambdaWithIfFalse :: Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.ConditionalExpression_TernaryLambda
conditionalExpression_TernaryLambdaWithIfFalse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifTrue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalExpression_TernaryLambda"),
              Core.projectionField = (Core.Name "ifTrue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifFalse"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

conditionalOrExpression :: Phantoms.TTerm [Syntax.ConditionalAndExpression] -> Phantoms.TTerm Syntax.ConditionalOrExpression
conditionalOrExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalOrExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unConditionalOrExpression :: Phantoms.TTerm Syntax.ConditionalOrExpression -> Phantoms.TTerm [Syntax.ConditionalAndExpression]
unConditionalOrExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ConditionalOrExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalAndExpression :: Phantoms.TTerm [Syntax.InclusiveOrExpression] -> Phantoms.TTerm Syntax.ConditionalAndExpression
conditionalAndExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ConditionalAndExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unConditionalAndExpression :: Phantoms.TTerm Syntax.ConditionalAndExpression -> Phantoms.TTerm [Syntax.InclusiveOrExpression]
unConditionalAndExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ConditionalAndExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inclusiveOrExpression :: Phantoms.TTerm [Syntax.ExclusiveOrExpression] -> Phantoms.TTerm Syntax.InclusiveOrExpression
inclusiveOrExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.InclusiveOrExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unInclusiveOrExpression :: Phantoms.TTerm Syntax.InclusiveOrExpression -> Phantoms.TTerm [Syntax.ExclusiveOrExpression]
unInclusiveOrExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.InclusiveOrExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exclusiveOrExpression :: Phantoms.TTerm [Syntax.AndExpression] -> Phantoms.TTerm Syntax.ExclusiveOrExpression
exclusiveOrExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ExclusiveOrExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unExclusiveOrExpression :: Phantoms.TTerm Syntax.ExclusiveOrExpression -> Phantoms.TTerm [Syntax.AndExpression]
unExclusiveOrExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ExclusiveOrExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

andExpression :: Phantoms.TTerm [Syntax.EqualityExpression] -> Phantoms.TTerm Syntax.AndExpression
andExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.AndExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAndExpression :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm [Syntax.EqualityExpression]
unAndExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.AndExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equalityExpressionUnary :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

equalityExpressionEqual :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

equalityExpressionNotEqual :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionNotEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

equalityExpression_Binary :: Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualityExpression_Binary
equalityExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

equalityExpression_BinaryLhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equalityExpression_BinaryRhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.RelationalExpression
equalityExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equalityExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.EqualityExpression_Binary
equalityExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equalityExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.EqualityExpression_Binary -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualityExpression_Binary
equalityExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.EqualityExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationalExpressionSimple :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionLessThan :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionLessThan x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionGreaterThan :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionGreaterThan x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionLessThanEqual :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionLessThanEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionGreaterThanEqual :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionGreaterThanEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionInstanceof :: Phantoms.TTerm Syntax.RelationalExpression_InstanceOf -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionInstanceof x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceof"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpression_LessThan :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThan
relationalExpression_LessThan lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

relationalExpression_LessThanLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_LessThanLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThan"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_LessThanRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_LessThanRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThan"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_LessThanWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThan"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationalExpression_LessThanWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThan -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThan
relationalExpression_LessThanWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThan"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationalExpression_GreaterThan :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThan lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

relationalExpression_GreaterThanLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_GreaterThanLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_GreaterThanRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_GreaterThanRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThan"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_GreaterThanWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThan"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationalExpression_GreaterThanWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThan -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThan
relationalExpression_GreaterThanWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThan"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThan"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationalExpression_LessThanEqual :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqual lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

relationalExpression_LessThanEqualLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_LessThanEqualLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_LessThanEqualRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_LessThanEqualRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThanEqual"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_LessThanEqualWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThanEqual"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationalExpression_LessThanEqualWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_LessThanEqual
relationalExpression_LessThanEqualWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_LessThanEqual"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationalExpression_GreaterThanEqual :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqual lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

relationalExpression_GreaterThanEqualLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_GreaterThanEqualLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_GreaterThanEqualRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.ShiftExpression
relationalExpression_GreaterThanEqualRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_GreaterThanEqualWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationalExpression_GreaterThanEqualWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression_GreaterThanEqual
relationalExpression_GreaterThanEqualWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationalExpression_InstanceOf :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.RelationalExpression_InstanceOf
relationalExpression_InstanceOf lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_InstanceOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

relationalExpression_InstanceOfLhs :: Phantoms.TTerm Syntax.RelationalExpression_InstanceOf -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpression_InstanceOfLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_InstanceOf"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_InstanceOfRhs :: Phantoms.TTerm Syntax.RelationalExpression_InstanceOf -> Phantoms.TTerm Syntax.ReferenceType
relationalExpression_InstanceOfRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_InstanceOf"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationalExpression_InstanceOfWithLhs :: Phantoms.TTerm Syntax.RelationalExpression_InstanceOf -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.RelationalExpression_InstanceOf
relationalExpression_InstanceOfWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_InstanceOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_InstanceOf"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationalExpression_InstanceOfWithRhs :: Phantoms.TTerm Syntax.RelationalExpression_InstanceOf -> Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.RelationalExpression_InstanceOf
relationalExpression_InstanceOfWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_InstanceOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.RelationalExpression_InstanceOf"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shiftExpressionUnary :: Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shiftExpressionShiftLeft :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionShiftLeft x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftLeft"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shiftExpressionShiftRight :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionShiftRight x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRight"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shiftExpressionShiftRightZeroFill :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionShiftRightZeroFill x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shiftRightZeroFill"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shiftExpression_Binary :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.ShiftExpression_Binary
shiftExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

shiftExpression_BinaryLhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shiftExpression_BinaryRhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
shiftExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shiftExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.ShiftExpression_Binary
shiftExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shiftExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.ShiftExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.ShiftExpression_Binary
shiftExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.ShiftExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

additiveExpressionUnary :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

additiveExpressionPlus :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionPlus x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

additiveExpressionMinus :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionMinus x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

additiveExpression_Binary :: Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AdditiveExpression_Binary
additiveExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

additiveExpression_BinaryLhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

additiveExpression_BinaryRhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
additiveExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

additiveExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

additiveExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.AdditiveExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AdditiveExpression_Binary
additiveExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.AdditiveExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiplicativeExpressionUnary :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplicativeExpressionTimes :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionTimes x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplicativeExpressionDivide :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionDivide x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplicativeExpressionMod :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionMod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplicativeExpression_Binary :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression_Binary
multiplicativeExpression_Binary lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

multiplicativeExpression_BinaryLhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpression_BinaryLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplicativeExpression_BinaryRhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.UnaryExpression
multiplicativeExpression_BinaryRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression_Binary"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplicativeExpression_BinaryWithLhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression_Binary"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiplicativeExpression_BinaryWithRhs :: Phantoms.TTerm Syntax.MultiplicativeExpression_Binary -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression_Binary
multiplicativeExpression_BinaryWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression_Binary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.MultiplicativeExpression_Binary"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryExpressionPreIncrement :: Phantoms.TTerm Syntax.PreIncrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionPreIncrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionPreDecrement :: Phantoms.TTerm Syntax.PreDecrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionPreDecrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionPlus :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionPlus x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionMinus :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionMinus x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionOther :: Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionOther x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preIncrementExpression :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.PreIncrementExpression
preIncrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.PreIncrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPreIncrementExpression :: Phantoms.TTerm Syntax.PreIncrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unPreIncrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.PreIncrementExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

preDecrementExpression :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.PreDecrementExpression
preDecrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.PreDecrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPreDecrementExpression :: Phantoms.TTerm Syntax.PreDecrementExpression -> Phantoms.TTerm Syntax.UnaryExpression
unPreDecrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.PreDecrementExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionNotPlusMinusPostfix :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusPostfix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionNotPlusMinusTilde :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusTilde x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tilde"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionNotPlusMinusNot :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusNot x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionNotPlusMinusCast :: Phantoms.TTerm Syntax.CastExpression -> Phantoms.TTerm Syntax.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusCast x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.UnaryExpressionNotPlusMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionPrimary :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionName :: Phantoms.TTerm Syntax.ExpressionName -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionPostIncrement :: Phantoms.TTerm Syntax.PostIncrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPostIncrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionPostDecrement :: Phantoms.TTerm Syntax.PostDecrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPostDecrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postIncrementExpression :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.PostIncrementExpression
postIncrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.PostIncrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPostIncrementExpression :: Phantoms.TTerm Syntax.PostIncrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
unPostIncrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.PostIncrementExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

postDecrementExpression :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.PostDecrementExpression
postDecrementExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.PostDecrementExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPostDecrementExpression :: Phantoms.TTerm Syntax.PostDecrementExpression -> Phantoms.TTerm Syntax.PostfixExpression
unPostDecrementExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.PostDecrementExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpressionPrimitive :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.CastExpression
castExpressionPrimitive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

castExpressionNotPlusMinus :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.CastExpression
castExpressionNotPlusMinus x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notPlusMinus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

castExpressionLambda :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.CastExpression
castExpressionLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

castExpression_Primitive :: Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_Primitive
castExpression_Primitive type_ expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

castExpression_PrimitiveType :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations
castExpression_PrimitiveType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Primitive"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_PrimitiveExpression :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.UnaryExpression
castExpression_PrimitiveExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Primitive"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_PrimitiveWithType :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.PrimitiveTypeWithAnnotations -> Phantoms.TTerm Syntax.CastExpression_Primitive
castExpression_PrimitiveWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Primitive"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

castExpression_PrimitiveWithExpression :: Phantoms.TTerm Syntax.CastExpression_Primitive -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_Primitive
castExpression_PrimitiveWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Primitive"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

castExpression_NotPlusMinus :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinus refAndBounds expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm refAndBounds)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

castExpression_NotPlusMinusRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_NotPlusMinusRefAndBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionField = (Core.Name "refAndBounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_NotPlusMinusExpression :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.UnaryExpression
castExpression_NotPlusMinusExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_NotPlusMinus"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_NotPlusMinusWithRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithRefAndBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_NotPlusMinus"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

castExpression_NotPlusMinusWithExpression :: Phantoms.TTerm Syntax.CastExpression_NotPlusMinus -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.CastExpression_NotPlusMinus
castExpression_NotPlusMinusWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_NotPlusMinus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_NotPlusMinus"),
              Core.projectionField = (Core.Name "refAndBounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

castExpression_Lambda :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CastExpression_Lambda
castExpression_Lambda refAndBounds expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm refAndBounds)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

castExpression_LambdaRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_LambdaRefAndBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Lambda"),
        Core.projectionField = (Core.Name "refAndBounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_LambdaExpression :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.LambdaExpression
castExpression_LambdaExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Lambda"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_LambdaWithRefAndBounds :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.CastExpression_Lambda
castExpression_LambdaWithRefAndBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Lambda"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

castExpression_LambdaWithExpression :: Phantoms.TTerm Syntax.CastExpression_Lambda -> Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CastExpression_Lambda
castExpression_LambdaWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "refAndBounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_Lambda"),
              Core.projectionField = (Core.Name "refAndBounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

castExpression_RefAndBounds :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_RefAndBounds type_ bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))

castExpression_RefAndBoundsType :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.ReferenceType
castExpression_RefAndBoundsType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_RefAndBoundsBounds :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm [Syntax.AdditionalBound]
castExpression_RefAndBoundsBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_RefAndBounds"),
        Core.projectionField = (Core.Name "bounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castExpression_RefAndBoundsWithType :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_RefAndBounds"),
              Core.projectionField = (Core.Name "bounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

castExpression_RefAndBoundsWithBounds :: Phantoms.TTerm Syntax.CastExpression_RefAndBounds -> Phantoms.TTerm [Syntax.AdditionalBound] -> Phantoms.TTerm Syntax.CastExpression_RefAndBounds
castExpression_RefAndBoundsWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_RefAndBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.syntax.CastExpression_RefAndBounds"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constantExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConstantExpression
constantExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.java.syntax.ConstantExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unConstantExpression :: Phantoms.TTerm Syntax.ConstantExpression -> Phantoms.TTerm Syntax.Expression
unConstantExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.java.syntax.ConstantExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
