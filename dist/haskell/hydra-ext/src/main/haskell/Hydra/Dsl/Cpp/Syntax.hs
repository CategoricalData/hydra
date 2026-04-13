-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.cpp.syntax

module Hydra.Dsl.Cpp.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Cpp.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

accessSpecifierNone :: Phantoms.TTerm Syntax.AccessSpecifier
accessSpecifierNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AccessSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

accessSpecifierPrivate :: Phantoms.TTerm Syntax.AccessSpecifier
accessSpecifierPrivate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AccessSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = Core.TermUnit}}))

accessSpecifierProtected :: Phantoms.TTerm Syntax.AccessSpecifier
accessSpecifierProtected =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AccessSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = Core.TermUnit}}))

accessSpecifierPublic :: Phantoms.TTerm Syntax.AccessSpecifier
accessSpecifierPublic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AccessSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "public"),
        Core.fieldTerm = Core.TermUnit}}))

addOperation :: Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AddOperation
addOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.AddOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

addOperationLeft :: Phantoms.TTerm Syntax.AddOperation -> Phantoms.TTerm Syntax.AdditiveExpression
addOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.AddOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

addOperationRight :: Phantoms.TTerm Syntax.AddOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
addOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.AddOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

addOperationWithLeft :: Phantoms.TTerm Syntax.AddOperation -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.AddOperation
addOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.AddOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.AddOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

addOperationWithRight :: Phantoms.TTerm Syntax.AddOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AddOperation
addOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.AddOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.AddOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

additiveExpressionAdd :: Phantoms.TTerm Syntax.AddOperation -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionAdd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

additiveExpressionMultiplicative :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionMultiplicative x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiplicative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

additiveExpressionSubtract :: Phantoms.TTerm Syntax.SubtractOperation -> Phantoms.TTerm Syntax.AdditiveExpression
additiveExpressionSubtract x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AdditiveExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

andExpressionBitwiseAnd :: Phantoms.TTerm Syntax.BitwiseAndOperation -> Phantoms.TTerm Syntax.AndExpression
andExpressionBitwiseAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AndExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

andExpressionEquality :: Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.AndExpression
andExpressionEquality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AndExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assignmentExpressionAssignment :: Phantoms.TTerm Syntax.ExplicitAssignment -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionAssignment x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assignmentExpressionConditional :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionConditional x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assignmentOperatorAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorBitwiseAndAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseAndAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAndAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorBitwiseOrAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseOrAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOrAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorBitwiseXorAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseXorAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXorAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorDivideAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorDivideAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divideAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorLeftShiftAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorLeftShiftAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorMinusAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorMinusAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minusAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorModuloAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorModuloAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "moduloAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorMultiplyAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorMultiplyAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiplyAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorPlusAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorPlusAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plusAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorRightShiftAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorRightShiftAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))

baseSpecifier :: Phantoms.TTerm Syntax.AccessSpecifier -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.BaseSpecifier
baseSpecifier access name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BaseSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "access"),
          Core.fieldTerm = (Phantoms.unTTerm access)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

baseSpecifierAccess :: Phantoms.TTerm Syntax.BaseSpecifier -> Phantoms.TTerm Syntax.AccessSpecifier
baseSpecifierAccess x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BaseSpecifier"),
        Core.projectionField = (Core.Name "access")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

baseSpecifierName :: Phantoms.TTerm Syntax.BaseSpecifier -> Phantoms.TTerm String
baseSpecifierName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BaseSpecifier"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

baseSpecifierWithAccess :: Phantoms.TTerm Syntax.BaseSpecifier -> Phantoms.TTerm Syntax.AccessSpecifier -> Phantoms.TTerm Syntax.BaseSpecifier
baseSpecifierWithAccess original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BaseSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "access"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BaseSpecifier"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

baseSpecifierWithName :: Phantoms.TTerm Syntax.BaseSpecifier -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.BaseSpecifier
baseSpecifierWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BaseSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "access"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BaseSpecifier"),
              Core.projectionField = (Core.Name "access")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

basicTypeAuto :: Phantoms.TTerm Syntax.BasicType
basicTypeAuto =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "auto"),
        Core.fieldTerm = Core.TermUnit}}))

basicTypeBool :: Phantoms.TTerm Syntax.BasicType
basicTypeBool =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bool"),
        Core.fieldTerm = Core.TermUnit}}))

basicTypeChar :: Phantoms.TTerm Syntax.BasicType
basicTypeChar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = Core.TermUnit}}))

basicTypeDouble :: Phantoms.TTerm Syntax.BasicType
basicTypeDouble =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))

basicTypeFloat :: Phantoms.TTerm Syntax.BasicType
basicTypeFloat =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))

basicTypeInt :: Phantoms.TTerm Syntax.BasicType
basicTypeInt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))

basicTypeNamed :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.BasicType
basicTypeNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

basicTypeString :: Phantoms.TTerm Syntax.BasicType
basicTypeString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

basicTypeVoid :: Phantoms.TTerm Syntax.BasicType
basicTypeVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BasicType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorBitwiseAnd :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorBitwiseAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorBitwiseOr :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorBitwiseOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorBitwiseXor :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorBitwiseXor =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorDivide :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorDivide =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorGreater :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorGreater =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greater"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorGreaterEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorGreaterEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLeftShift :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLeftShift =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLess :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLess =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "less"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLessEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLessEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLogicalAnd :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLogicalAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "logicalAnd"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLogicalOr :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLogicalOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "logicalOr"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorMinus :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorMinus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorModulo :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorModulo =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modulo"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorMultiply :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorMultiply =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorNotEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorNotEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorPlus :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorPlus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorRightShift :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorRightShift =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = Core.TermUnit}}))

bitwiseAndOperation :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.BitwiseAndOperation
bitwiseAndOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseAndOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

bitwiseAndOperationLeft :: Phantoms.TTerm Syntax.BitwiseAndOperation -> Phantoms.TTerm Syntax.AndExpression
bitwiseAndOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseAndOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bitwiseAndOperationRight :: Phantoms.TTerm Syntax.BitwiseAndOperation -> Phantoms.TTerm Syntax.EqualityExpression
bitwiseAndOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseAndOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bitwiseAndOperationWithLeft :: Phantoms.TTerm Syntax.BitwiseAndOperation -> Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm Syntax.BitwiseAndOperation
bitwiseAndOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseAndOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseAndOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bitwiseAndOperationWithRight :: Phantoms.TTerm Syntax.BitwiseAndOperation -> Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.BitwiseAndOperation
bitwiseAndOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseAndOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseAndOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

bitwiseOrOperation :: Phantoms.TTerm Syntax.InclusiveOrExpression -> Phantoms.TTerm Syntax.ExclusiveOrExpression -> Phantoms.TTerm Syntax.BitwiseOrOperation
bitwiseOrOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseOrOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

bitwiseOrOperationLeft :: Phantoms.TTerm Syntax.BitwiseOrOperation -> Phantoms.TTerm Syntax.InclusiveOrExpression
bitwiseOrOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseOrOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bitwiseOrOperationRight :: Phantoms.TTerm Syntax.BitwiseOrOperation -> Phantoms.TTerm Syntax.ExclusiveOrExpression
bitwiseOrOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseOrOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bitwiseOrOperationWithLeft :: Phantoms.TTerm Syntax.BitwiseOrOperation -> Phantoms.TTerm Syntax.InclusiveOrExpression -> Phantoms.TTerm Syntax.BitwiseOrOperation
bitwiseOrOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseOrOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseOrOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bitwiseOrOperationWithRight :: Phantoms.TTerm Syntax.BitwiseOrOperation -> Phantoms.TTerm Syntax.ExclusiveOrExpression -> Phantoms.TTerm Syntax.BitwiseOrOperation
bitwiseOrOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseOrOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseOrOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

bitwiseXorOperation :: Phantoms.TTerm Syntax.ExclusiveOrExpression -> Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm Syntax.BitwiseXorOperation
bitwiseXorOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseXorOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

bitwiseXorOperationLeft :: Phantoms.TTerm Syntax.BitwiseXorOperation -> Phantoms.TTerm Syntax.ExclusiveOrExpression
bitwiseXorOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseXorOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bitwiseXorOperationRight :: Phantoms.TTerm Syntax.BitwiseXorOperation -> Phantoms.TTerm Syntax.AndExpression
bitwiseXorOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseXorOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bitwiseXorOperationWithLeft :: Phantoms.TTerm Syntax.BitwiseXorOperation -> Phantoms.TTerm Syntax.ExclusiveOrExpression -> Phantoms.TTerm Syntax.BitwiseXorOperation
bitwiseXorOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseXorOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseXorOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bitwiseXorOperationWithRight :: Phantoms.TTerm Syntax.BitwiseXorOperation -> Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm Syntax.BitwiseXorOperation
bitwiseXorOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.BitwiseXorOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.BitwiseXorOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

booleanLiteral :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.BooleanLiteral
booleanLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.BooleanLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

capture :: Phantoms.TTerm String -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Capture
capture name byReference =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Capture"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "byReference"),
          Core.fieldTerm = (Phantoms.unTTerm byReference)}]}))

captureByReference :: Phantoms.TTerm Syntax.Capture -> Phantoms.TTerm Bool
captureByReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Capture"),
        Core.projectionField = (Core.Name "byReference")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

captureListCaptureByValue :: Phantoms.TTerm Syntax.CaptureList
captureListCaptureByValue =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.CaptureList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "captureByValue"),
        Core.fieldTerm = Core.TermUnit}}))

captureListCaptures :: Phantoms.TTerm [Syntax.Capture] -> Phantoms.TTerm Syntax.CaptureList
captureListCaptures x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.CaptureList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "captures"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

captureName :: Phantoms.TTerm Syntax.Capture -> Phantoms.TTerm String
captureName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Capture"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

captureWithByReference :: Phantoms.TTerm Syntax.Capture -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Capture
captureWithByReference original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Capture"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Capture"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "byReference"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

captureWithName :: Phantoms.TTerm Syntax.Capture -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Capture
captureWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Capture"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "byReference"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Capture"),
              Core.projectionField = (Core.Name "byReference")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseStatementCase :: Phantoms.TTerm Syntax.CaseValue -> Phantoms.TTerm Syntax.CaseStatement
caseStatementCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.CaseStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseStatementDefault :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.CaseStatement
caseStatementDefault x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.CaseStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseValue :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.CaseValue
caseValue value statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.CaseValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))

caseValueStatement :: Phantoms.TTerm Syntax.CaseValue -> Phantoms.TTerm Syntax.Statement
caseValueStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CaseValue"),
        Core.projectionField = (Core.Name "statement")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseValueValue :: Phantoms.TTerm Syntax.CaseValue -> Phantoms.TTerm Syntax.Expression
caseValueValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CaseValue"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseValueWithStatement :: Phantoms.TTerm Syntax.CaseValue -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.CaseValue
caseValueWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.CaseValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CaseValue"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseValueWithValue :: Phantoms.TTerm Syntax.CaseValue -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseValue
caseValueWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.CaseValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CaseValue"),
              Core.projectionField = (Core.Name "statement")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

characterLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.CharacterLiteral
characterLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.CharacterLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

classBody :: Phantoms.TTerm [Syntax.MemberSpecification] -> Phantoms.TTerm Syntax.ClassBody
classBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.ClassBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

classDeclaration :: Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclaration specifier body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifier"),
          Core.fieldTerm = (Phantoms.unTTerm specifier)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

classDeclarationBody :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm (Maybe Syntax.ClassBody)
classDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDeclarationSpecifier :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.ClassSpecifier
classDeclarationSpecifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassDeclaration"),
        Core.projectionField = (Core.Name "specifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDeclarationWithBody :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm (Maybe Syntax.ClassBody) -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "specifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classDeclarationWithSpecifier :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationWithSpecifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classKeyClass :: Phantoms.TTerm Syntax.ClassKey
classKeyClass =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ClassKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = Core.TermUnit}}))

classKeyEnum :: Phantoms.TTerm Syntax.ClassKey
classKeyEnum =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ClassKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = Core.TermUnit}}))

classKeyEnumClass :: Phantoms.TTerm Syntax.ClassKey
classKeyEnumClass =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ClassKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumClass"),
        Core.fieldTerm = Core.TermUnit}}))

classKeyStruct :: Phantoms.TTerm Syntax.ClassKey
classKeyStruct =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ClassKey"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = Core.TermUnit}}))

classSpecifier :: Phantoms.TTerm Syntax.ClassKey -> Phantoms.TTerm String -> Phantoms.TTerm [Syntax.BaseSpecifier] -> Phantoms.TTerm Syntax.ClassSpecifier
classSpecifier key name inheritance =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "inheritance"),
          Core.fieldTerm = (Phantoms.unTTerm inheritance)}]}))

classSpecifierInheritance :: Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm [Syntax.BaseSpecifier]
classSpecifierInheritance x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
        Core.projectionField = (Core.Name "inheritance")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classSpecifierKey :: Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm Syntax.ClassKey
classSpecifierKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classSpecifierName :: Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm String
classSpecifierName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classSpecifierWithInheritance :: Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm [Syntax.BaseSpecifier] -> Phantoms.TTerm Syntax.ClassSpecifier
classSpecifierWithInheritance original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inheritance"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classSpecifierWithKey :: Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm Syntax.ClassKey -> Phantoms.TTerm Syntax.ClassSpecifier
classSpecifierWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inheritance"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
              Core.projectionField = (Core.Name "inheritance")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classSpecifierWithName :: Phantoms.TTerm Syntax.ClassSpecifier -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ClassSpecifier
classSpecifierWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inheritance"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ClassSpecifier"),
              Core.projectionField = (Core.Name "inheritance")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

commaExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.CommaExpression
commaExpression left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.CommaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

commaExpressionLeft :: Phantoms.TTerm Syntax.CommaExpression -> Phantoms.TTerm Syntax.Expression
commaExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CommaExpression"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commaExpressionRight :: Phantoms.TTerm Syntax.CommaExpression -> Phantoms.TTerm Syntax.AssignmentExpression
commaExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CommaExpression"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commaExpressionWithLeft :: Phantoms.TTerm Syntax.CommaExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CommaExpression
commaExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.CommaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CommaExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

commaExpressionWithRight :: Phantoms.TTerm Syntax.CommaExpression -> Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.CommaExpression
commaExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.CommaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.CommaExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

comment :: Phantoms.TTerm String -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Comment
comment text isMultiline =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Phantoms.unTTerm text)},
        Core.Field {
          Core.fieldName = (Core.Name "isMultiline"),
          Core.fieldTerm = (Phantoms.unTTerm isMultiline)}]}))

commentIsMultiline :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm Bool
commentIsMultiline x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Comment"),
        Core.projectionField = (Core.Name "isMultiline")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commentText :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm String
commentText x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Comment"),
        Core.projectionField = (Core.Name "text")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commentWithIsMultiline :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Comment
commentWithIsMultiline original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Comment"),
              Core.projectionField = (Core.Name "text")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isMultiline"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

commentWithText :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
commentWithText original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isMultiline"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Comment"),
              Core.projectionField = (Core.Name "isMultiline")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

compoundStatement :: Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.CompoundStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

conditionalExpressionLogicalOr :: Phantoms.TTerm Syntax.LogicalOrExpression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionLogicalOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "logicalOr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

conditionalExpressionTernary :: Phantoms.TTerm Syntax.TernaryExpression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionTernary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ConditionalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ternary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constructorDeclaration :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm [Syntax.MemInitializer] -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclaration name parameters initializers body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "initializers"),
          Core.fieldTerm = (Phantoms.unTTerm initializers)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

constructorDeclarationBody :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.FunctionBody
constructorDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationInitializers :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.MemInitializer]
constructorDeclarationInitializers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "initializers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationName :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm String
constructorDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationParameters :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.Parameter]
constructorDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorDeclarationWithBody :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "initializers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constructorDeclarationWithInitializers :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.MemInitializer] -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithInitializers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclarationWithName :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "initializers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorDeclarationWithParameters :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm Syntax.ConstructorDeclaration
constructorDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "initializers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "initializers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ConstructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

containerDeclarationList :: Phantoms.TTerm Syntax.ListDeclaration -> Phantoms.TTerm Syntax.ContainerDeclaration
containerDeclarationList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ContainerDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

containerDeclarationMap :: Phantoms.TTerm Syntax.MapDeclaration -> Phantoms.TTerm Syntax.ContainerDeclaration
containerDeclarationMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ContainerDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

containerDeclarationOptional :: Phantoms.TTerm Syntax.OptionalDeclaration -> Phantoms.TTerm Syntax.ContainerDeclaration
containerDeclarationOptional x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ContainerDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

containerDeclarationSet :: Phantoms.TTerm Syntax.SetDeclaration -> Phantoms.TTerm Syntax.ContainerDeclaration
containerDeclarationSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ContainerDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationFunction :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationNamespace :: Phantoms.TTerm Syntax.NamespaceDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationNamespace x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "namespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationPreprocessor :: Phantoms.TTerm Syntax.PreprocessorDirective -> Phantoms.TTerm Syntax.Declaration
declarationPreprocessor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preprocessor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationTemplate :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationTemplate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationTypedef :: Phantoms.TTerm Syntax.TypedefDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationTypedef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedef"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationVariable :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defineDirective :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe [String]) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DefineDirective
defineDirective name parameters replacement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "replacement"),
          Core.fieldTerm = (Phantoms.unTTerm replacement)}]}))

defineDirectiveName :: Phantoms.TTerm Syntax.DefineDirective -> Phantoms.TTerm String
defineDirectiveName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defineDirectiveParameters :: Phantoms.TTerm Syntax.DefineDirective -> Phantoms.TTerm (Maybe [String])
defineDirectiveParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defineDirectiveReplacement :: Phantoms.TTerm Syntax.DefineDirective -> Phantoms.TTerm (Maybe String)
defineDirectiveReplacement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
        Core.projectionField = (Core.Name "replacement")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defineDirectiveWithName :: Phantoms.TTerm Syntax.DefineDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DefineDirective
defineDirectiveWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "replacement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
              Core.projectionField = (Core.Name "replacement")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defineDirectiveWithParameters :: Phantoms.TTerm Syntax.DefineDirective -> Phantoms.TTerm (Maybe [String]) -> Phantoms.TTerm Syntax.DefineDirective
defineDirectiveWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "replacement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
              Core.projectionField = (Core.Name "replacement")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defineDirectiveWithReplacement :: Phantoms.TTerm Syntax.DefineDirective -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DefineDirective
defineDirectiveWithReplacement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DefineDirective"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "replacement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

destructorDeclaration :: Phantoms.TTerm [Syntax.FunctionSpecifierPrefix] -> Phantoms.TTerm String -> Phantoms.TTerm [Syntax.FunctionSpecifierSuffix] -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.DestructorDeclaration
destructorDeclaration prefixSpecifiers name suffixSpecifiers body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm prefixSpecifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm suffixSpecifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

destructorDeclarationBody :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm Syntax.FunctionBody
destructorDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

destructorDeclarationName :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm String
destructorDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

destructorDeclarationPrefixSpecifiers :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierPrefix]
destructorDeclarationPrefixSpecifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
        Core.projectionField = (Core.Name "prefixSpecifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

destructorDeclarationSuffixSpecifiers :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierSuffix]
destructorDeclarationSuffixSpecifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
        Core.projectionField = (Core.Name "suffixSpecifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

destructorDeclarationWithBody :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.DestructorDeclaration
destructorDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

destructorDeclarationWithName :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DestructorDeclaration
destructorDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

destructorDeclarationWithPrefixSpecifiers :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierPrefix] -> Phantoms.TTerm Syntax.DestructorDeclaration
destructorDeclarationWithPrefixSpecifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

destructorDeclarationWithSuffixSpecifiers :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierSuffix] -> Phantoms.TTerm Syntax.DestructorDeclaration
destructorDeclarationWithSuffixSpecifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DestructorDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

divideOperation :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.DivideOperation
divideOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DivideOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

divideOperationLeft :: Phantoms.TTerm Syntax.DivideOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
divideOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DivideOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

divideOperationRight :: Phantoms.TTerm Syntax.DivideOperation -> Phantoms.TTerm Syntax.UnaryExpression
divideOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DivideOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

divideOperationWithLeft :: Phantoms.TTerm Syntax.DivideOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.DivideOperation
divideOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DivideOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DivideOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

divideOperationWithRight :: Phantoms.TTerm Syntax.DivideOperation -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.DivideOperation
divideOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DivideOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DivideOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

doStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DoStatement
doStatement body condition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)}]}))

doStatementBody :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Statement
doStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DoStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doStatementCondition :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Expression
doStatementCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DoStatement"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doStatementWithBody :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.DoStatement
doStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DoStatement"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

doStatementWithCondition :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DoStatement
doStatementWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.DoStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.DoStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elifDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ElifDirective
elifDirective condition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ElifDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)}]}))

elifDirectiveCondition :: Phantoms.TTerm Syntax.ElifDirective -> Phantoms.TTerm String
elifDirectiveCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ElifDirective"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elifDirectiveWithCondition :: Phantoms.TTerm Syntax.ElifDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ElifDirective
elifDirectiveWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ElifDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equalOperation :: Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualOperation
equalOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.EqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

equalOperationLeft :: Phantoms.TTerm Syntax.EqualOperation -> Phantoms.TTerm Syntax.EqualityExpression
equalOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.EqualOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equalOperationRight :: Phantoms.TTerm Syntax.EqualOperation -> Phantoms.TTerm Syntax.RelationalExpression
equalOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.EqualOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equalOperationWithLeft :: Phantoms.TTerm Syntax.EqualOperation -> Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.EqualOperation
equalOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.EqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.EqualOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equalOperationWithRight :: Phantoms.TTerm Syntax.EqualOperation -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualOperation
equalOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.EqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.EqualOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equalityExpressionEqual :: Phantoms.TTerm Syntax.EqualOperation -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

equalityExpressionNotEqual :: Phantoms.TTerm Syntax.NotEqualOperation -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionNotEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

equalityExpressionRelational :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.EqualityExpression
equalityExpressionRelational x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.EqualityExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "relational"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ErrorDirective
errorDirective message =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ErrorDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)}]}))

errorDirectiveMessage :: Phantoms.TTerm Syntax.ErrorDirective -> Phantoms.TTerm String
errorDirectiveMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ErrorDirective"),
        Core.projectionField = (Core.Name "message")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

errorDirectiveWithMessage :: Phantoms.TTerm Syntax.ErrorDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ErrorDirective
errorDirectiveWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ErrorDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

exclusiveOrExpressionAnd :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm Syntax.ExclusiveOrExpression
exclusiveOrExpressionAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ExclusiveOrExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exclusiveOrExpressionBitwiseXor :: Phantoms.TTerm Syntax.BitwiseXorOperation -> Phantoms.TTerm Syntax.ExclusiveOrExpression
exclusiveOrExpressionBitwiseXor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ExclusiveOrExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

explicitAssignment :: Phantoms.TTerm Syntax.LogicalOrExpression -> Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.ExplicitAssignment
explicitAssignment left op right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

explicitAssignmentLeft :: Phantoms.TTerm Syntax.ExplicitAssignment -> Phantoms.TTerm Syntax.LogicalOrExpression
explicitAssignmentLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitAssignmentOp :: Phantoms.TTerm Syntax.ExplicitAssignment -> Phantoms.TTerm Syntax.AssignmentOperator
explicitAssignmentOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitAssignmentRight :: Phantoms.TTerm Syntax.ExplicitAssignment -> Phantoms.TTerm Syntax.AssignmentExpression
explicitAssignmentRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitAssignmentWithLeft :: Phantoms.TTerm Syntax.ExplicitAssignment -> Phantoms.TTerm Syntax.LogicalOrExpression -> Phantoms.TTerm Syntax.ExplicitAssignment
explicitAssignmentWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

explicitAssignmentWithOp :: Phantoms.TTerm Syntax.ExplicitAssignment -> Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.ExplicitAssignment
explicitAssignmentWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

explicitAssignmentWithRight :: Phantoms.TTerm Syntax.ExplicitAssignment -> Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.ExplicitAssignment
explicitAssignmentWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ExplicitAssignment"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

expressionAssignment :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression
expressionAssignment x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionComma :: Phantoms.TTerm Syntax.CommaExpression -> Phantoms.TTerm Syntax.Expression
expressionComma x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "comma"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExpressionStatement
expressionStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.ExpressionStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

floatingLiteral :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatingLiteral
floatingLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.FloatingLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

forInitDeclaration :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.ForInit
forInitDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forInitEmpty :: Phantoms.TTerm Syntax.ForInit
forInitEmpty =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))

forInitExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForInit
forInitExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forStatement :: Phantoms.TTerm Syntax.ForInit -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForStatement
forStatement init condition increment body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Phantoms.unTTerm increment)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

forStatementBody :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Statement
forStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementCondition :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Expression
forStatementCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementIncrement :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Expression
forStatementIncrement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
        Core.projectionField = (Core.Name "increment")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementInit :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.ForInit
forStatementInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementWithBody :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForStatement
forStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "increment")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forStatementWithCondition :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForStatement
forStatementWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "increment")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forStatementWithIncrement :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForStatement
forStatementWithIncrement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forStatementWithInit :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.ForInit -> Phantoms.TTerm Syntax.ForStatement
forStatementWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "increment")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionApplication :: Phantoms.TTerm Syntax.FunctionIdentifier -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionApplication
functionApplication function arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

functionApplicationArguments :: Phantoms.TTerm Syntax.FunctionApplication -> Phantoms.TTerm [Syntax.Expression]
functionApplicationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionApplication"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionApplicationFunction :: Phantoms.TTerm Syntax.FunctionApplication -> Phantoms.TTerm Syntax.FunctionIdentifier
functionApplicationFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionApplication"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionApplicationWithArguments :: Phantoms.TTerm Syntax.FunctionApplication -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionApplication
functionApplicationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionApplication"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionApplicationWithFunction :: Phantoms.TTerm Syntax.FunctionApplication -> Phantoms.TTerm Syntax.FunctionIdentifier -> Phantoms.TTerm Syntax.FunctionApplication
functionApplicationWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionApplication"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionBodyCompound :: Phantoms.TTerm Syntax.CompoundStatement -> Phantoms.TTerm Syntax.FunctionBody
functionBodyCompound x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionBodyDeclaration :: Phantoms.TTerm Syntax.FunctionBody
functionBodyDeclaration =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = Core.TermUnit}}))

functionBodyDefault :: Phantoms.TTerm Syntax.FunctionBody
functionBodyDefault =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))

functionBodyPure :: Phantoms.TTerm Syntax.FunctionBody
functionBodyPure =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pure"),
        Core.fieldTerm = Core.TermUnit}}))

functionCallOperation :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionCallOperation
functionCallOperation function arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionCallOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

functionCallOperationArguments :: Phantoms.TTerm Syntax.FunctionCallOperation -> Phantoms.TTerm [Syntax.Expression]
functionCallOperationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionCallOperation"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionCallOperationFunction :: Phantoms.TTerm Syntax.FunctionCallOperation -> Phantoms.TTerm Syntax.PostfixExpression
functionCallOperationFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionCallOperation"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionCallOperationWithArguments :: Phantoms.TTerm Syntax.FunctionCallOperation -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionCallOperation
functionCallOperationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionCallOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionCallOperation"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionCallOperationWithFunction :: Phantoms.TTerm Syntax.FunctionCallOperation -> Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.FunctionCallOperation
functionCallOperationWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionCallOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionCallOperation"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclaration :: Phantoms.TTerm [Syntax.FunctionSpecifierPrefix] -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm String -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm [Syntax.FunctionSpecifierSuffix] -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclaration prefixSpecifiers returnType name parameters suffixSpecifiers body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm prefixSpecifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm suffixSpecifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

functionDeclarationBody :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.FunctionBody
functionDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationName :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm String
functionDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationParameters :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.Parameter]
functionDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationPrefixSpecifiers :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierPrefix]
functionDeclarationPrefixSpecifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "prefixSpecifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationReturnType :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.TypeExpression
functionDeclarationReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "returnType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationSuffixSpecifiers :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierSuffix]
functionDeclarationSuffixSpecifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "suffixSpecifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationWithBody :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionDeclarationWithName :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithParameters :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithPrefixSpecifiers :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierPrefix] -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithPrefixSpecifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithReturnType :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "suffixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithSuffixSpecifiers :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.FunctionSpecifierSuffix] -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithSuffixSpecifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixSpecifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "prefixSpecifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "suffixSpecifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionIdentifierQualified :: Phantoms.TTerm Syntax.QualifiedIdentifier -> Phantoms.TTerm Syntax.FunctionIdentifier
functionIdentifierQualified x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionIdentifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualified"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionIdentifierSimple :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.FunctionIdentifier
functionIdentifierSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionIdentifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionSpecifierPrefixExplicit :: Phantoms.TTerm Syntax.FunctionSpecifierPrefix
functionSpecifierPrefixExplicit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = Core.TermUnit}}))

functionSpecifierPrefixInline :: Phantoms.TTerm Syntax.FunctionSpecifierPrefix
functionSpecifierPrefixInline =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inline"),
        Core.fieldTerm = Core.TermUnit}}))

functionSpecifierPrefixStatic :: Phantoms.TTerm Syntax.FunctionSpecifierPrefix
functionSpecifierPrefixStatic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "static"),
        Core.fieldTerm = Core.TermUnit}}))

functionSpecifierPrefixVirtual :: Phantoms.TTerm Syntax.FunctionSpecifierPrefix
functionSpecifierPrefixVirtual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "virtual"),
        Core.fieldTerm = Core.TermUnit}}))

functionSpecifierSuffixConst :: Phantoms.TTerm Syntax.FunctionSpecifierSuffix
functionSpecifierSuffixConst =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierSuffix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = Core.TermUnit}}))

functionSpecifierSuffixFinal :: Phantoms.TTerm Syntax.FunctionSpecifierSuffix
functionSpecifierSuffixFinal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierSuffix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

functionSpecifierSuffixNoexcept :: Phantoms.TTerm Syntax.FunctionSpecifierSuffix
functionSpecifierSuffixNoexcept =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierSuffix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noexcept"),
        Core.fieldTerm = Core.TermUnit}}))

functionSpecifierSuffixOverride :: Phantoms.TTerm Syntax.FunctionSpecifierSuffix
functionSpecifierSuffixOverride =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionSpecifierSuffix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "override"),
        Core.fieldTerm = Core.TermUnit}}))

functionType :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm Syntax.FunctionType
functionType returnType parameters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)}]}))

functionTypeParameters :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm [Syntax.Parameter]
functionTypeParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionType"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeReturnType :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.TypeExpression
functionTypeReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionType"),
        Core.projectionField = (Core.Name "returnType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeWithParameters :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionType"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionTypeWithReturnType :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.FunctionType"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

greaterEqualOperation :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.GreaterEqualOperation
greaterEqualOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.GreaterEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

greaterEqualOperationLeft :: Phantoms.TTerm Syntax.GreaterEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression
greaterEqualOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterEqualOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

greaterEqualOperationRight :: Phantoms.TTerm Syntax.GreaterEqualOperation -> Phantoms.TTerm Syntax.ShiftExpression
greaterEqualOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterEqualOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

greaterEqualOperationWithLeft :: Phantoms.TTerm Syntax.GreaterEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.GreaterEqualOperation
greaterEqualOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.GreaterEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterEqualOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

greaterEqualOperationWithRight :: Phantoms.TTerm Syntax.GreaterEqualOperation -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.GreaterEqualOperation
greaterEqualOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.GreaterEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterEqualOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

greaterOperation :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.GreaterOperation
greaterOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.GreaterOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

greaterOperationLeft :: Phantoms.TTerm Syntax.GreaterOperation -> Phantoms.TTerm Syntax.RelationalExpression
greaterOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

greaterOperationRight :: Phantoms.TTerm Syntax.GreaterOperation -> Phantoms.TTerm Syntax.ShiftExpression
greaterOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

greaterOperationWithLeft :: Phantoms.TTerm Syntax.GreaterOperation -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.GreaterOperation
greaterOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.GreaterOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

greaterOperationWithRight :: Phantoms.TTerm Syntax.GreaterOperation -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.GreaterOperation
greaterOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.GreaterOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.GreaterOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IfDirective
ifDirective condition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IfDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)}]}))

ifDirectiveCondition :: Phantoms.TTerm Syntax.IfDirective -> Phantoms.TTerm String
ifDirectiveCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.IfDirective"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifDirectiveWithCondition :: Phantoms.TTerm Syntax.IfDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.IfDirective
ifDirectiveWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IfDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifdefDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IfdefDirective
ifdefDirective identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IfdefDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))

ifdefDirectiveIdentifier :: Phantoms.TTerm Syntax.IfdefDirective -> Phantoms.TTerm String
ifdefDirectiveIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.IfdefDirective"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifdefDirectiveWithIdentifier :: Phantoms.TTerm Syntax.IfdefDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.IfdefDirective
ifdefDirectiveWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IfdefDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifndefDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IfndefDirective
ifndefDirective identifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IfndefDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm identifier)}]}))

ifndefDirectiveIdentifier :: Phantoms.TTerm Syntax.IfndefDirective -> Phantoms.TTerm String
ifndefDirectiveIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.IfndefDirective"),
        Core.projectionField = (Core.Name "identifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifndefDirectiveWithIdentifier :: Phantoms.TTerm Syntax.IfndefDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.IfndefDirective
ifndefDirectiveWithIdentifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IfndefDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

includeDirective :: Phantoms.TTerm String -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IncludeDirective
includeDirective name isSystem =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IncludeDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "isSystem"),
          Core.fieldTerm = (Phantoms.unTTerm isSystem)}]}))

includeDirectiveIsSystem :: Phantoms.TTerm Syntax.IncludeDirective -> Phantoms.TTerm Bool
includeDirectiveIsSystem x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.IncludeDirective"),
        Core.projectionField = (Core.Name "isSystem")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

includeDirectiveName :: Phantoms.TTerm Syntax.IncludeDirective -> Phantoms.TTerm String
includeDirectiveName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.IncludeDirective"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

includeDirectiveWithIsSystem :: Phantoms.TTerm Syntax.IncludeDirective -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IncludeDirective
includeDirectiveWithIsSystem original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IncludeDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.IncludeDirective"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isSystem"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

includeDirectiveWithName :: Phantoms.TTerm Syntax.IncludeDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.IncludeDirective
includeDirectiveWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.IncludeDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isSystem"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.IncludeDirective"),
              Core.projectionField = (Core.Name "isSystem")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inclusiveOrExpressionBitwiseOr :: Phantoms.TTerm Syntax.BitwiseOrOperation -> Phantoms.TTerm Syntax.InclusiveOrExpression
inclusiveOrExpressionBitwiseOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.InclusiveOrExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inclusiveOrExpressionExclusiveOr :: Phantoms.TTerm Syntax.ExclusiveOrExpression -> Phantoms.TTerm Syntax.InclusiveOrExpression
inclusiveOrExpressionExclusiveOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.InclusiveOrExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exclusiveOr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerLiteralBinary :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IntegerLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerLiteralDecimal :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralDecimal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IntegerLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerLiteralHexadecimal :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralHexadecimal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IntegerLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hexadecimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerLiteralOctal :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralOctal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IntegerLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "octal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iterationStatementDo :: Phantoms.TTerm Syntax.DoStatement -> Phantoms.TTerm Syntax.IterationStatement
iterationStatementDo x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IterationStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iterationStatementFor :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.IterationStatement
iterationStatementFor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IterationStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iterationStatementRangeFor :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm Syntax.IterationStatement
iterationStatementRangeFor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IterationStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rangeFor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iterationStatementWhile :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.IterationStatement
iterationStatementWhile x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.IterationStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

jumpStatementBreak :: Phantoms.TTerm Syntax.JumpStatement
jumpStatementBreak =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.JumpStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = Core.TermUnit}}))

jumpStatementContinue :: Phantoms.TTerm Syntax.JumpStatement
jumpStatementContinue =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.JumpStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = Core.TermUnit}}))

jumpStatementReturnValue :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.JumpStatement
jumpStatementReturnValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.JumpStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "returnValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

jumpStatementReturnVoid :: Phantoms.TTerm Syntax.JumpStatement
jumpStatementReturnVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.JumpStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "returnVoid"),
        Core.fieldTerm = Core.TermUnit}}))

jumpStatementThrow :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.JumpStatement
jumpStatementThrow x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.JumpStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

labeledStatement :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatement label statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))

labeledStatementLabel :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm String
labeledStatementLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementStatement :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
labeledStatementStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "statement")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementWithLabel :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "statement")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

labeledStatementWithStatement :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaExpression :: Phantoms.TTerm Syntax.CaptureList -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm Syntax.CompoundStatement -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpression captures parameters returnType body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "captures"),
          Core.fieldTerm = (Phantoms.unTTerm captures)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

lambdaExpressionBody :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CompoundStatement
lambdaExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionCaptures :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CaptureList
lambdaExpressionCaptures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "captures")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionParameters :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm [Syntax.Parameter]
lambdaExpressionParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionReturnType :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm (Maybe Syntax.TypeExpression)
lambdaExpressionReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "returnType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionWithBody :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CompoundStatement -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "captures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "captures")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaExpressionWithCaptures :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.CaptureList -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithCaptures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "captures"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaExpressionWithParameters :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm [Syntax.Parameter] -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "captures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "captures")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaExpressionWithReturnType :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "captures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "captures")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

leftShiftOperation :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.LeftShiftOperation
leftShiftOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LeftShiftOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

leftShiftOperationLeft :: Phantoms.TTerm Syntax.LeftShiftOperation -> Phantoms.TTerm Syntax.ShiftExpression
leftShiftOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LeftShiftOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

leftShiftOperationRight :: Phantoms.TTerm Syntax.LeftShiftOperation -> Phantoms.TTerm Syntax.AdditiveExpression
leftShiftOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LeftShiftOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

leftShiftOperationWithLeft :: Phantoms.TTerm Syntax.LeftShiftOperation -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.LeftShiftOperation
leftShiftOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LeftShiftOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LeftShiftOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

leftShiftOperationWithRight :: Phantoms.TTerm Syntax.LeftShiftOperation -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.LeftShiftOperation
leftShiftOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LeftShiftOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LeftShiftOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lessEqualOperation :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.LessEqualOperation
lessEqualOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LessEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

lessEqualOperationLeft :: Phantoms.TTerm Syntax.LessEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression
lessEqualOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessEqualOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lessEqualOperationRight :: Phantoms.TTerm Syntax.LessEqualOperation -> Phantoms.TTerm Syntax.ShiftExpression
lessEqualOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessEqualOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lessEqualOperationWithLeft :: Phantoms.TTerm Syntax.LessEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.LessEqualOperation
lessEqualOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LessEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessEqualOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lessEqualOperationWithRight :: Phantoms.TTerm Syntax.LessEqualOperation -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.LessEqualOperation
lessEqualOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LessEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessEqualOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lessOperation :: Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.LessOperation
lessOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

lessOperationLeft :: Phantoms.TTerm Syntax.LessOperation -> Phantoms.TTerm Syntax.RelationalExpression
lessOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lessOperationRight :: Phantoms.TTerm Syntax.LessOperation -> Phantoms.TTerm Syntax.ShiftExpression
lessOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lessOperationWithLeft :: Phantoms.TTerm Syntax.LessOperation -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.LessOperation
lessOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lessOperationWithRight :: Phantoms.TTerm Syntax.LessOperation -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.LessOperation
lessOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LessOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lineDirective :: Phantoms.TTerm Int -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.LineDirective
lineDirective lineNumber filename =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LineDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lineNumber"),
          Core.fieldTerm = (Phantoms.unTTerm lineNumber)},
        Core.Field {
          Core.fieldName = (Core.Name "filename"),
          Core.fieldTerm = (Phantoms.unTTerm filename)}]}))

lineDirectiveFilename :: Phantoms.TTerm Syntax.LineDirective -> Phantoms.TTerm (Maybe String)
lineDirectiveFilename x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LineDirective"),
        Core.projectionField = (Core.Name "filename")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lineDirectiveLineNumber :: Phantoms.TTerm Syntax.LineDirective -> Phantoms.TTerm Int
lineDirectiveLineNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LineDirective"),
        Core.projectionField = (Core.Name "lineNumber")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lineDirectiveWithFilename :: Phantoms.TTerm Syntax.LineDirective -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.LineDirective
lineDirectiveWithFilename original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LineDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lineNumber"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LineDirective"),
              Core.projectionField = (Core.Name "lineNumber")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "filename"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lineDirectiveWithLineNumber :: Phantoms.TTerm Syntax.LineDirective -> Phantoms.TTerm Int -> Phantoms.TTerm Syntax.LineDirective
lineDirectiveWithLineNumber original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LineDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lineNumber"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "filename"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LineDirective"),
              Core.projectionField = (Core.Name "filename")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

listDeclaration :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ListDeclaration
listDeclaration elementType name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ListDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm elementType)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

listDeclarationElementType :: Phantoms.TTerm Syntax.ListDeclaration -> Phantoms.TTerm Syntax.TypeExpression
listDeclarationElementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ListDeclaration"),
        Core.projectionField = (Core.Name "elementType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

listDeclarationName :: Phantoms.TTerm Syntax.ListDeclaration -> Phantoms.TTerm String
listDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ListDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

listDeclarationWithElementType :: Phantoms.TTerm Syntax.ListDeclaration -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.ListDeclaration
listDeclarationWithElementType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ListDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ListDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

listDeclarationWithName :: Phantoms.TTerm Syntax.ListDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ListDeclaration
listDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ListDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ListDeclaration"),
              Core.projectionField = (Core.Name "elementType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

literalBoolean :: Phantoms.TTerm Syntax.BooleanLiteral -> Phantoms.TTerm Syntax.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalCharacter :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm Syntax.Literal
literalCharacter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalFloating :: Phantoms.TTerm Syntax.FloatingLiteral -> Phantoms.TTerm Syntax.Literal
literalFloating x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floating"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInteger :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalNull :: Phantoms.TTerm Syntax.Literal
literalNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

literalString :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalAndExpressionInclusiveOr :: Phantoms.TTerm Syntax.InclusiveOrExpression -> Phantoms.TTerm Syntax.LogicalAndExpression
logicalAndExpressionInclusiveOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inclusiveOr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalAndExpressionLogicalAnd :: Phantoms.TTerm Syntax.LogicalAndOperation -> Phantoms.TTerm Syntax.LogicalAndExpression
logicalAndExpressionLogicalAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "logicalAnd"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalAndOperation :: Phantoms.TTerm Syntax.LogicalAndExpression -> Phantoms.TTerm Syntax.InclusiveOrExpression -> Phantoms.TTerm Syntax.LogicalAndOperation
logicalAndOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

logicalAndOperationLeft :: Phantoms.TTerm Syntax.LogicalAndOperation -> Phantoms.TTerm Syntax.LogicalAndExpression
logicalAndOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

logicalAndOperationRight :: Phantoms.TTerm Syntax.LogicalAndOperation -> Phantoms.TTerm Syntax.InclusiveOrExpression
logicalAndOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

logicalAndOperationWithLeft :: Phantoms.TTerm Syntax.LogicalAndOperation -> Phantoms.TTerm Syntax.LogicalAndExpression -> Phantoms.TTerm Syntax.LogicalAndOperation
logicalAndOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

logicalAndOperationWithRight :: Phantoms.TTerm Syntax.LogicalAndOperation -> Phantoms.TTerm Syntax.InclusiveOrExpression -> Phantoms.TTerm Syntax.LogicalAndOperation
logicalAndOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalAndOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

logicalOrExpressionLogicalAnd :: Phantoms.TTerm Syntax.LogicalAndExpression -> Phantoms.TTerm Syntax.LogicalOrExpression
logicalOrExpressionLogicalAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "logicalAnd"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalOrExpressionLogicalOr :: Phantoms.TTerm Syntax.LogicalOrOperation -> Phantoms.TTerm Syntax.LogicalOrExpression
logicalOrExpressionLogicalOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "logicalOr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalOrOperation :: Phantoms.TTerm Syntax.LogicalOrExpression -> Phantoms.TTerm Syntax.LogicalAndExpression -> Phantoms.TTerm Syntax.LogicalOrOperation
logicalOrOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

logicalOrOperationLeft :: Phantoms.TTerm Syntax.LogicalOrOperation -> Phantoms.TTerm Syntax.LogicalOrExpression
logicalOrOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

logicalOrOperationRight :: Phantoms.TTerm Syntax.LogicalOrOperation -> Phantoms.TTerm Syntax.LogicalAndExpression
logicalOrOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

logicalOrOperationWithLeft :: Phantoms.TTerm Syntax.LogicalOrOperation -> Phantoms.TTerm Syntax.LogicalOrExpression -> Phantoms.TTerm Syntax.LogicalOrOperation
logicalOrOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

logicalOrOperationWithRight :: Phantoms.TTerm Syntax.LogicalOrOperation -> Phantoms.TTerm Syntax.LogicalAndExpression -> Phantoms.TTerm Syntax.LogicalOrOperation
logicalOrOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.LogicalOrOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

map :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm [Syntax.MapEntry] -> Phantoms.TTerm Syntax.Map
map keyType valueType entries =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Map"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Phantoms.unTTerm keyType)},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm valueType)},
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Phantoms.unTTerm entries)}]}))

mapDeclaration :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MapDeclaration
mapDeclaration keyType valueType name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Phantoms.unTTerm keyType)},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm valueType)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

mapDeclarationKeyType :: Phantoms.TTerm Syntax.MapDeclaration -> Phantoms.TTerm Syntax.TypeExpression
mapDeclarationKeyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
        Core.projectionField = (Core.Name "keyType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapDeclarationName :: Phantoms.TTerm Syntax.MapDeclaration -> Phantoms.TTerm String
mapDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapDeclarationValueType :: Phantoms.TTerm Syntax.MapDeclaration -> Phantoms.TTerm Syntax.TypeExpression
mapDeclarationValueType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
        Core.projectionField = (Core.Name "valueType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapDeclarationWithKeyType :: Phantoms.TTerm Syntax.MapDeclaration -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.MapDeclaration
mapDeclarationWithKeyType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
              Core.projectionField = (Core.Name "valueType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mapDeclarationWithName :: Phantoms.TTerm Syntax.MapDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MapDeclaration
mapDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
              Core.projectionField = (Core.Name "keyType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
              Core.projectionField = (Core.Name "valueType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mapDeclarationWithValueType :: Phantoms.TTerm Syntax.MapDeclaration -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.MapDeclaration
mapDeclarationWithValueType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
              Core.projectionField = (Core.Name "keyType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mapEntries :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm [Syntax.MapEntry]
mapEntries x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
        Core.projectionField = (Core.Name "entries")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapEntry :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntry key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

mapEntryKey :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression
mapEntryKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapEntry"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapEntryValue :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression
mapEntryValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapEntry"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapEntryWithKey :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntryWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapEntry"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mapEntryWithValue :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntryWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MapEntry"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mapKeyType :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm Syntax.TypeExpression
mapKeyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
        Core.projectionField = (Core.Name "keyType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapValueType :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm Syntax.TypeExpression
mapValueType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
        Core.projectionField = (Core.Name "valueType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapWithEntries :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm [Syntax.MapEntry] -> Phantoms.TTerm Syntax.Map
mapWithEntries original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Map"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
              Core.projectionField = (Core.Name "keyType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
              Core.projectionField = (Core.Name "valueType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mapWithKeyType :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.Map
mapWithKeyType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Map"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
              Core.projectionField = (Core.Name "valueType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
              Core.projectionField = (Core.Name "entries")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mapWithValueType :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.Map
mapWithValueType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Map"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
              Core.projectionField = (Core.Name "keyType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Map"),
              Core.projectionField = (Core.Name "entries")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memInitializer :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MemInitializer
memInitializer name arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MemInitializer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

memInitializerArguments :: Phantoms.TTerm Syntax.MemInitializer -> Phantoms.TTerm [Syntax.Expression]
memInitializerArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemInitializer"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memInitializerName :: Phantoms.TTerm Syntax.MemInitializer -> Phantoms.TTerm String
memInitializerName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemInitializer"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memInitializerWithArguments :: Phantoms.TTerm Syntax.MemInitializer -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MemInitializer
memInitializerWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MemInitializer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemInitializer"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

memInitializerWithName :: Phantoms.TTerm Syntax.MemInitializer -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MemInitializer
memInitializerWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MemInitializer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemInitializer"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memberAccessOperation :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MemberAccessOperation
memberAccessOperation object member =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MemberAccessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "member"),
          Core.fieldTerm = (Phantoms.unTTerm member)}]}))

memberAccessOperationMember :: Phantoms.TTerm Syntax.MemberAccessOperation -> Phantoms.TTerm String
memberAccessOperationMember x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemberAccessOperation"),
        Core.projectionField = (Core.Name "member")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memberAccessOperationObject :: Phantoms.TTerm Syntax.MemberAccessOperation -> Phantoms.TTerm Syntax.PostfixExpression
memberAccessOperationObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemberAccessOperation"),
        Core.projectionField = (Core.Name "object")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memberAccessOperationWithMember :: Phantoms.TTerm Syntax.MemberAccessOperation -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.MemberAccessOperation
memberAccessOperationWithMember original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MemberAccessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemberAccessOperation"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "member"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

memberAccessOperationWithObject :: Phantoms.TTerm Syntax.MemberAccessOperation -> Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.MemberAccessOperation
memberAccessOperationWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MemberAccessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "member"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MemberAccessOperation"),
              Core.projectionField = (Core.Name "member")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memberDeclarationConstructor :: Phantoms.TTerm Syntax.ConstructorDeclaration -> Phantoms.TTerm Syntax.MemberDeclaration
memberDeclarationConstructor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberDeclarationDestructor :: Phantoms.TTerm Syntax.DestructorDeclaration -> Phantoms.TTerm Syntax.MemberDeclaration
memberDeclarationDestructor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberDeclarationFunction :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.MemberDeclaration
memberDeclarationFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberDeclarationNestedClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.MemberDeclaration
memberDeclarationNestedClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedClass"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberDeclarationTemplate :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm Syntax.MemberDeclaration
memberDeclarationTemplate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberDeclarationVariable :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.MemberDeclaration
memberDeclarationVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberSpecificationAccessLabel :: Phantoms.TTerm Syntax.AccessSpecifier -> Phantoms.TTerm Syntax.MemberSpecification
memberSpecificationAccessLabel x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberSpecification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "accessLabel"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberSpecificationMember :: Phantoms.TTerm Syntax.MemberDeclaration -> Phantoms.TTerm Syntax.MemberSpecification
memberSpecificationMember x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MemberSpecification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "member"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduloOperation :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.ModuloOperation
moduloOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ModuloOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

moduloOperationLeft :: Phantoms.TTerm Syntax.ModuloOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
moduloOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ModuloOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduloOperationRight :: Phantoms.TTerm Syntax.ModuloOperation -> Phantoms.TTerm Syntax.UnaryExpression
moduloOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ModuloOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduloOperationWithLeft :: Phantoms.TTerm Syntax.ModuloOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.ModuloOperation
moduloOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ModuloOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ModuloOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduloOperationWithRight :: Phantoms.TTerm Syntax.ModuloOperation -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.ModuloOperation
moduloOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ModuloOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ModuloOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiplicativeExpressionDivide :: Phantoms.TTerm Syntax.DivideOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionDivide x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplicativeExpressionModulo :: Phantoms.TTerm Syntax.ModuloOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionModulo x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modulo"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplicativeExpressionMultiply :: Phantoms.TTerm Syntax.MultiplyOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionMultiply x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplicativeExpressionUnary :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplicativeExpressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplicativeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiplyOperation :: Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplyOperation
multiplyOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MultiplyOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

multiplyOperationLeft :: Phantoms.TTerm Syntax.MultiplyOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
multiplyOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplyOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplyOperationRight :: Phantoms.TTerm Syntax.MultiplyOperation -> Phantoms.TTerm Syntax.UnaryExpression
multiplyOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplyOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplyOperationWithLeft :: Phantoms.TTerm Syntax.MultiplyOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.MultiplyOperation
multiplyOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MultiplyOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplyOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiplyOperationWithRight :: Phantoms.TTerm Syntax.MultiplyOperation -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.MultiplyOperation
multiplyOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.MultiplyOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.MultiplyOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namespaceDeclaration :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.Declaration] -> Phantoms.TTerm Syntax.NamespaceDeclaration
namespaceDeclaration name declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.NamespaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm declarations)}]}))

namespaceDeclarationDeclarations :: Phantoms.TTerm Syntax.NamespaceDeclaration -> Phantoms.TTerm [Syntax.Declaration]
namespaceDeclarationDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NamespaceDeclaration"),
        Core.projectionField = (Core.Name "declarations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namespaceDeclarationName :: Phantoms.TTerm Syntax.NamespaceDeclaration -> Phantoms.TTerm String
namespaceDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NamespaceDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namespaceDeclarationWithDeclarations :: Phantoms.TTerm Syntax.NamespaceDeclaration -> Phantoms.TTerm [Syntax.Declaration] -> Phantoms.TTerm Syntax.NamespaceDeclaration
namespaceDeclarationWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.NamespaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NamespaceDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namespaceDeclarationWithName :: Phantoms.TTerm Syntax.NamespaceDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.NamespaceDeclaration
namespaceDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.NamespaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NamespaceDeclaration"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

notEqualOperation :: Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.NotEqualOperation
notEqualOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.NotEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

notEqualOperationLeft :: Phantoms.TTerm Syntax.NotEqualOperation -> Phantoms.TTerm Syntax.EqualityExpression
notEqualOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NotEqualOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notEqualOperationRight :: Phantoms.TTerm Syntax.NotEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression
notEqualOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NotEqualOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notEqualOperationWithLeft :: Phantoms.TTerm Syntax.NotEqualOperation -> Phantoms.TTerm Syntax.EqualityExpression -> Phantoms.TTerm Syntax.NotEqualOperation
notEqualOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.NotEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NotEqualOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

notEqualOperationWithRight :: Phantoms.TTerm Syntax.NotEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression -> Phantoms.TTerm Syntax.NotEqualOperation
notEqualOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.NotEqualOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.NotEqualOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

optional :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Optional
optional valueType value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Optional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm valueType)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

optionalDeclaration :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.OptionalDeclaration
optionalDeclaration valueType name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.OptionalDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm valueType)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

optionalDeclarationName :: Phantoms.TTerm Syntax.OptionalDeclaration -> Phantoms.TTerm String
optionalDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.OptionalDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalDeclarationValueType :: Phantoms.TTerm Syntax.OptionalDeclaration -> Phantoms.TTerm Syntax.TypeExpression
optionalDeclarationValueType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.OptionalDeclaration"),
        Core.projectionField = (Core.Name "valueType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalDeclarationWithName :: Phantoms.TTerm Syntax.OptionalDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.OptionalDeclaration
optionalDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.OptionalDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.OptionalDeclaration"),
              Core.projectionField = (Core.Name "valueType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

optionalDeclarationWithValueType :: Phantoms.TTerm Syntax.OptionalDeclaration -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.OptionalDeclaration
optionalDeclarationWithValueType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.OptionalDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.OptionalDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

optionalValue :: Phantoms.TTerm Syntax.Optional -> Phantoms.TTerm (Maybe Syntax.Expression)
optionalValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Optional"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalValueType :: Phantoms.TTerm Syntax.Optional -> Phantoms.TTerm Syntax.TypeExpression
optionalValueType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Optional"),
        Core.projectionField = (Core.Name "valueType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

optionalWithValue :: Phantoms.TTerm Syntax.Optional -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Optional
optionalWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Optional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Optional"),
              Core.projectionField = (Core.Name "valueType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

optionalWithValueType :: Phantoms.TTerm Syntax.Optional -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.Optional
optionalWithValueType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Optional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "valueType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Optional"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

overloadedLambdas :: Phantoms.TTerm [Syntax.LambdaExpression] -> Phantoms.TTerm Syntax.OverloadedLambdas
overloadedLambdas x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.OverloadedLambdas"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

parameter :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm String -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Parameter
parameter type_ name unnamed defaultValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "unnamed"),
          Core.fieldTerm = (Phantoms.unTTerm unnamed)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm defaultValue)}]}))

parameterDefaultValue :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm (Maybe Syntax.Expression)
parameterDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
        Core.projectionField = (Core.Name "defaultValue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterName :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm String
parameterName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterType :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm Syntax.TypeExpression
parameterType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterUnnamed :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm Bool
parameterUnnamed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
        Core.projectionField = (Core.Name "unnamed")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterWithDefaultValue :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Parameter
parameterWithDefaultValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unnamed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "unnamed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parameterWithName :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Parameter
parameterWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unnamed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "unnamed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parameterWithType :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.Parameter
parameterWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unnamed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "unnamed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parameterWithUnnamed :: Phantoms.TTerm Syntax.Parameter -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Parameter
parameterWithUnnamed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unnamed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Parameter"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternMatch :: Phantoms.TTerm Syntax.Visitor -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PatternMatch
patternMatch visitor variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PatternMatch"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "visitor"),
          Core.fieldTerm = (Phantoms.unTTerm visitor)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))

patternMatchVariant :: Phantoms.TTerm Syntax.PatternMatch -> Phantoms.TTerm Syntax.Expression
patternMatchVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PatternMatch"),
        Core.projectionField = (Core.Name "variant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternMatchVisitor :: Phantoms.TTerm Syntax.PatternMatch -> Phantoms.TTerm Syntax.Visitor
patternMatchVisitor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PatternMatch"),
        Core.projectionField = (Core.Name "visitor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternMatchWithVariant :: Phantoms.TTerm Syntax.PatternMatch -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PatternMatch
patternMatchWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PatternMatch"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "visitor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PatternMatch"),
              Core.projectionField = (Core.Name "visitor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternMatchWithVisitor :: Phantoms.TTerm Syntax.PatternMatch -> Phantoms.TTerm Syntax.Visitor -> Phantoms.TTerm Syntax.PatternMatch
patternMatchWithVisitor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PatternMatch"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "visitor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PatternMatch"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pointerMemberAccessOperation :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.PointerMemberAccessOperation
pointerMemberAccessOperation pointer member =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PointerMemberAccessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Phantoms.unTTerm pointer)},
        Core.Field {
          Core.fieldName = (Core.Name "member"),
          Core.fieldTerm = (Phantoms.unTTerm member)}]}))

pointerMemberAccessOperationMember :: Phantoms.TTerm Syntax.PointerMemberAccessOperation -> Phantoms.TTerm String
pointerMemberAccessOperationMember x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PointerMemberAccessOperation"),
        Core.projectionField = (Core.Name "member")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pointerMemberAccessOperationPointer :: Phantoms.TTerm Syntax.PointerMemberAccessOperation -> Phantoms.TTerm Syntax.PostfixExpression
pointerMemberAccessOperationPointer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PointerMemberAccessOperation"),
        Core.projectionField = (Core.Name "pointer")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pointerMemberAccessOperationWithMember :: Phantoms.TTerm Syntax.PointerMemberAccessOperation -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.PointerMemberAccessOperation
pointerMemberAccessOperationWithMember original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PointerMemberAccessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PointerMemberAccessOperation"),
              Core.projectionField = (Core.Name "pointer")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "member"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pointerMemberAccessOperationWithPointer :: Phantoms.TTerm Syntax.PointerMemberAccessOperation -> Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.PointerMemberAccessOperation
pointerMemberAccessOperationWithPointer original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PointerMemberAccessOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "member"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PointerMemberAccessOperation"),
              Core.projectionField = (Core.Name "member")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

postfixExpressionFunctionCall :: Phantoms.TTerm Syntax.FunctionCallOperation -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionFunctionCall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionCall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionMemberAccess :: Phantoms.TTerm Syntax.MemberAccessOperation -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionMemberAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "memberAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionPointerMemberAccess :: Phantoms.TTerm Syntax.PointerMemberAccessOperation -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPointerMemberAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pointerMemberAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionPostDecrement :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPostDecrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postDecrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionPostIncrement :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPostIncrement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postIncrement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionPrimary :: Phantoms.TTerm Syntax.PrimaryExpression -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionSubscript :: Phantoms.TTerm Syntax.SubscriptOperation -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionSubscript x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subscript"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

postfixExpressionTemplateFunctionCall :: Phantoms.TTerm Syntax.TemplateFunctionCallOperation -> Phantoms.TTerm Syntax.PostfixExpression
postfixExpressionTemplateFunctionCall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PostfixExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "templateFunctionCall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pragmaDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PragmaDirective
pragmaDirective content =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PragmaDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Phantoms.unTTerm content)}]}))

pragmaDirectiveContent :: Phantoms.TTerm Syntax.PragmaDirective -> Phantoms.TTerm String
pragmaDirectiveContent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.PragmaDirective"),
        Core.projectionField = (Core.Name "content")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pragmaDirectiveWithContent :: Phantoms.TTerm Syntax.PragmaDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.PragmaDirective
pragmaDirectiveWithContent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.PragmaDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

preprocessorDirectiveDefine :: Phantoms.TTerm Syntax.DefineDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveDefine x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "define"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveElif :: Phantoms.TTerm Syntax.ElifDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveElif x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elif"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveElse :: Phantoms.TTerm Syntax.ElseDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveElse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "else"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveEndif :: Phantoms.TTerm Syntax.EndifDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveEndif x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endif"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveError :: Phantoms.TTerm Syntax.ErrorDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveError x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "error"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveIf :: Phantoms.TTerm Syntax.IfDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveIfdef :: Phantoms.TTerm Syntax.IfdefDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveIfdef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifdef"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveIfndef :: Phantoms.TTerm Syntax.IfndefDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveIfndef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ifndef"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveInclude :: Phantoms.TTerm Syntax.IncludeDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveInclude x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "include"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveLine :: Phantoms.TTerm Syntax.LineDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveLine x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "line"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectivePragma :: Phantoms.TTerm Syntax.PragmaDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectivePragma x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pragma"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveUndef :: Phantoms.TTerm Syntax.UndefDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveUndef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undef"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

preprocessorDirectiveWarning :: Phantoms.TTerm Syntax.WarningDirective -> Phantoms.TTerm Syntax.PreprocessorDirective
preprocessorDirectiveWarning x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PreprocessorDirective"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "warning"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryExpressionIdentifier :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PrimaryExpression
primaryExpressionIdentifier x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PrimaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryExpressionLambda :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.PrimaryExpression
primaryExpressionLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PrimaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryExpressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.PrimaryExpression
primaryExpressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PrimaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryExpressionParenthesized :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PrimaryExpression
primaryExpressionParenthesized x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.PrimaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

productDeclaration :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.VariableDeclaration] -> Phantoms.TTerm Syntax.ProductDeclaration
productDeclaration name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ProductDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

productDeclarationFields :: Phantoms.TTerm Syntax.ProductDeclaration -> Phantoms.TTerm [Syntax.VariableDeclaration]
productDeclarationFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ProductDeclaration"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

productDeclarationName :: Phantoms.TTerm Syntax.ProductDeclaration -> Phantoms.TTerm String
productDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ProductDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

productDeclarationWithFields :: Phantoms.TTerm Syntax.ProductDeclaration -> Phantoms.TTerm [Syntax.VariableDeclaration] -> Phantoms.TTerm Syntax.ProductDeclaration
productDeclarationWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ProductDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ProductDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

productDeclarationWithName :: Phantoms.TTerm Syntax.ProductDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ProductDeclaration
productDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.ProductDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.ProductDeclaration"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

program :: Phantoms.TTerm [Syntax.PreprocessorDirective] -> Phantoms.TTerm [Syntax.IncludeDirective] -> Phantoms.TTerm [Syntax.Declaration] -> Phantoms.TTerm Syntax.Program
program preprocessorDirectives includes declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "preprocessorDirectives"),
          Core.fieldTerm = (Phantoms.unTTerm preprocessorDirectives)},
        Core.Field {
          Core.fieldName = (Core.Name "includes"),
          Core.fieldTerm = (Phantoms.unTTerm includes)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm declarations)}]}))

programDeclarations :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.Declaration]
programDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
        Core.projectionField = (Core.Name "declarations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programIncludes :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.IncludeDirective]
programIncludes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
        Core.projectionField = (Core.Name "includes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programPreprocessorDirectives :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.PreprocessorDirective]
programPreprocessorDirectives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
        Core.projectionField = (Core.Name "preprocessorDirectives")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programWithDeclarations :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.Declaration] -> Phantoms.TTerm Syntax.Program
programWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "preprocessorDirectives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
              Core.projectionField = (Core.Name "preprocessorDirectives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
              Core.projectionField = (Core.Name "includes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

programWithIncludes :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.IncludeDirective] -> Phantoms.TTerm Syntax.Program
programWithIncludes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "preprocessorDirectives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
              Core.projectionField = (Core.Name "preprocessorDirectives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "includes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

programWithPreprocessorDirectives :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.PreprocessorDirective] -> Phantoms.TTerm Syntax.Program
programWithPreprocessorDirectives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "preprocessorDirectives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "includes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
              Core.projectionField = (Core.Name "includes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Program"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualifiedIdentifier :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedIdentifier
qualifiedIdentifier namespace name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.QualifiedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

qualifiedIdentifierName :: Phantoms.TTerm Syntax.QualifiedIdentifier -> Phantoms.TTerm String
qualifiedIdentifierName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedIdentifier"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedIdentifierNamespace :: Phantoms.TTerm Syntax.QualifiedIdentifier -> Phantoms.TTerm String
qualifiedIdentifierNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedIdentifier"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedIdentifierWithName :: Phantoms.TTerm Syntax.QualifiedIdentifier -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedIdentifier
qualifiedIdentifierWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.QualifiedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedIdentifier"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualifiedIdentifierWithNamespace :: Phantoms.TTerm Syntax.QualifiedIdentifier -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedIdentifier
qualifiedIdentifierWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.QualifiedIdentifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedIdentifier"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualifiedType :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.TypeQualifier -> Phantoms.TTerm Syntax.QualifiedType
qualifiedType baseType qualifier =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.QualifiedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "baseType"),
          Core.fieldTerm = (Phantoms.unTTerm baseType)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm qualifier)}]}))

qualifiedTypeBaseType :: Phantoms.TTerm Syntax.QualifiedType -> Phantoms.TTerm Syntax.TypeExpression
qualifiedTypeBaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedType"),
        Core.projectionField = (Core.Name "baseType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedTypeQualifier :: Phantoms.TTerm Syntax.QualifiedType -> Phantoms.TTerm Syntax.TypeQualifier
qualifiedTypeQualifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedType"),
        Core.projectionField = (Core.Name "qualifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedTypeWithBaseType :: Phantoms.TTerm Syntax.QualifiedType -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.QualifiedType
qualifiedTypeWithBaseType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.QualifiedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "baseType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedType"),
              Core.projectionField = (Core.Name "qualifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualifiedTypeWithQualifier :: Phantoms.TTerm Syntax.QualifiedType -> Phantoms.TTerm Syntax.TypeQualifier -> Phantoms.TTerm Syntax.QualifiedType
qualifiedTypeWithQualifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.QualifiedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "baseType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.QualifiedType"),
              Core.projectionField = (Core.Name "baseType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangeForStatement :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.RangeForStatement
rangeForStatement type_ variable range body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

rangeForStatementBody :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm Syntax.Statement
rangeForStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeForStatementRange :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm Syntax.Expression
rangeForStatementRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeForStatementType :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm Syntax.TypeExpression
rangeForStatementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeForStatementVariable :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm String
rangeForStatementVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
        Core.projectionField = (Core.Name "variable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeForStatementWithBody :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.RangeForStatement
rangeForStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangeForStatementWithRange :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RangeForStatement
rangeForStatementWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangeForStatementWithType :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.RangeForStatement
rangeForStatementWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangeForStatementWithVariable :: Phantoms.TTerm Syntax.RangeForStatement -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.RangeForStatement
rangeForStatementWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RangeForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationalExpressionGreater :: Phantoms.TTerm Syntax.GreaterOperation -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionGreater x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greater"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionGreaterEqual :: Phantoms.TTerm Syntax.GreaterEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionGreaterEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionLess :: Phantoms.TTerm Syntax.LessOperation -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionLess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "less"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionLessEqual :: Phantoms.TTerm Syntax.LessEqualOperation -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionLessEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationalExpressionShift :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RelationalExpression
relationalExpressionShift x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.RelationalExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shift"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rightShiftOperation :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.RightShiftOperation
rightShiftOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RightShiftOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

rightShiftOperationLeft :: Phantoms.TTerm Syntax.RightShiftOperation -> Phantoms.TTerm Syntax.ShiftExpression
rightShiftOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RightShiftOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rightShiftOperationRight :: Phantoms.TTerm Syntax.RightShiftOperation -> Phantoms.TTerm Syntax.AdditiveExpression
rightShiftOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RightShiftOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rightShiftOperationWithLeft :: Phantoms.TTerm Syntax.RightShiftOperation -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.RightShiftOperation
rightShiftOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RightShiftOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RightShiftOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rightShiftOperationWithRight :: Phantoms.TTerm Syntax.RightShiftOperation -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.RightShiftOperation
rightShiftOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.RightShiftOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.RightShiftOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectionStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm (Maybe Syntax.Statement) -> Phantoms.TTerm Syntax.SelectionStatement
selectionStatement condition thenBranch elseBranch =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "thenBranch"),
          Core.fieldTerm = (Phantoms.unTTerm thenBranch)},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Phantoms.unTTerm elseBranch)}]}))

selectionStatementCondition :: Phantoms.TTerm Syntax.SelectionStatement -> Phantoms.TTerm Syntax.Expression
selectionStatementCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectionStatementElseBranch :: Phantoms.TTerm Syntax.SelectionStatement -> Phantoms.TTerm (Maybe Syntax.Statement)
selectionStatementElseBranch x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
        Core.projectionField = (Core.Name "elseBranch")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectionStatementThenBranch :: Phantoms.TTerm Syntax.SelectionStatement -> Phantoms.TTerm Syntax.Statement
selectionStatementThenBranch x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
        Core.projectionField = (Core.Name "thenBranch")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectionStatementWithCondition :: Phantoms.TTerm Syntax.SelectionStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SelectionStatement
selectionStatementWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thenBranch"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
              Core.projectionField = (Core.Name "thenBranch")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
              Core.projectionField = (Core.Name "elseBranch")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

selectionStatementWithElseBranch :: Phantoms.TTerm Syntax.SelectionStatement -> Phantoms.TTerm (Maybe Syntax.Statement) -> Phantoms.TTerm Syntax.SelectionStatement
selectionStatementWithElseBranch original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenBranch"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
              Core.projectionField = (Core.Name "thenBranch")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectionStatementWithThenBranch :: Phantoms.TTerm Syntax.SelectionStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.SelectionStatement
selectionStatementWithThenBranch original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenBranch"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elseBranch"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SelectionStatement"),
              Core.projectionField = (Core.Name "elseBranch")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

set :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Set
set elementType elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Set"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm elementType)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

setDeclaration :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.SetDeclaration
setDeclaration elementType name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SetDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm elementType)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

setDeclarationElementType :: Phantoms.TTerm Syntax.SetDeclaration -> Phantoms.TTerm Syntax.TypeExpression
setDeclarationElementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SetDeclaration"),
        Core.projectionField = (Core.Name "elementType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

setDeclarationName :: Phantoms.TTerm Syntax.SetDeclaration -> Phantoms.TTerm String
setDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SetDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

setDeclarationWithElementType :: Phantoms.TTerm Syntax.SetDeclaration -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.SetDeclaration
setDeclarationWithElementType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SetDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SetDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

setDeclarationWithName :: Phantoms.TTerm Syntax.SetDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.SetDeclaration
setDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SetDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SetDeclaration"),
              Core.projectionField = (Core.Name "elementType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

setElementType :: Phantoms.TTerm Syntax.Set -> Phantoms.TTerm Syntax.TypeExpression
setElementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Set"),
        Core.projectionField = (Core.Name "elementType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

setElements :: Phantoms.TTerm Syntax.Set -> Phantoms.TTerm [Syntax.Expression]
setElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Set"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

setWithElementType :: Phantoms.TTerm Syntax.Set -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.Set
setWithElementType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Set"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Set"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

setWithElements :: Phantoms.TTerm Syntax.Set -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Set
setWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Set"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Set"),
              Core.projectionField = (Core.Name "elementType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shiftExpressionAdditive :: Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionAdditive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "additive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shiftExpressionLeftShift :: Phantoms.TTerm Syntax.LeftShiftOperation -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionLeftShift x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shiftExpressionRightShift :: Phantoms.TTerm Syntax.RightShiftOperation -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionRightShift x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.ShiftExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sizeofExpression :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.SizeofExpression
sizeofExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.SizeofExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

statementCompound :: Phantoms.TTerm Syntax.CompoundStatement -> Phantoms.TTerm Syntax.Statement
statementCompound x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementDeclaration :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.Statement
statementDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement
statementExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementIteration :: Phantoms.TTerm Syntax.IterationStatement -> Phantoms.TTerm Syntax.Statement
statementIteration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iteration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementJump :: Phantoms.TTerm Syntax.JumpStatement -> Phantoms.TTerm Syntax.Statement
statementJump x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "jump"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementLabeled :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
statementLabeled x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementSelection :: Phantoms.TTerm Syntax.SelectionStatement -> Phantoms.TTerm Syntax.Statement
statementSelection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementSwitch :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Statement
statementSwitch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteral
stringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cpp.syntax.StringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

subscriptOperation :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SubscriptOperation
subscriptOperation array index =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SubscriptOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm array)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm index)}]}))

subscriptOperationArray :: Phantoms.TTerm Syntax.SubscriptOperation -> Phantoms.TTerm Syntax.PostfixExpression
subscriptOperationArray x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubscriptOperation"),
        Core.projectionField = (Core.Name "array")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subscriptOperationIndex :: Phantoms.TTerm Syntax.SubscriptOperation -> Phantoms.TTerm Syntax.Expression
subscriptOperationIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubscriptOperation"),
        Core.projectionField = (Core.Name "index")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subscriptOperationWithArray :: Phantoms.TTerm Syntax.SubscriptOperation -> Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.SubscriptOperation
subscriptOperationWithArray original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SubscriptOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubscriptOperation"),
              Core.projectionField = (Core.Name "index")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subscriptOperationWithIndex :: Phantoms.TTerm Syntax.SubscriptOperation -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SubscriptOperation
subscriptOperationWithIndex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SubscriptOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubscriptOperation"),
              Core.projectionField = (Core.Name "array")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subtractOperation :: Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.SubtractOperation
subtractOperation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SubtractOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

subtractOperationLeft :: Phantoms.TTerm Syntax.SubtractOperation -> Phantoms.TTerm Syntax.AdditiveExpression
subtractOperationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubtractOperation"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtractOperationRight :: Phantoms.TTerm Syntax.SubtractOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression
subtractOperationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubtractOperation"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtractOperationWithLeft :: Phantoms.TTerm Syntax.SubtractOperation -> Phantoms.TTerm Syntax.AdditiveExpression -> Phantoms.TTerm Syntax.SubtractOperation
subtractOperationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SubtractOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubtractOperation"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subtractOperationWithRight :: Phantoms.TTerm Syntax.SubtractOperation -> Phantoms.TTerm Syntax.MultiplicativeExpression -> Phantoms.TTerm Syntax.SubtractOperation
subtractOperationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SubtractOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SubtractOperation"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.CaseStatement] -> Phantoms.TTerm Syntax.SwitchStatement
switchStatement value cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

switchStatementCases :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm [Syntax.CaseStatement]
switchStatementCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchStatementValue :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression
switchStatementValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchStatementWithCases :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm [Syntax.CaseStatement] -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchStatementWithValue :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateArgumentType :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.TemplateArgument
templateArgumentType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

templateArgumentValue :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TemplateArgument
templateArgumentValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateArgument"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

templateDeclaration :: Phantoms.TTerm Bool -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.TemplateDeclaration
templateDeclaration inline parameters declaration =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inline"),
          Core.fieldTerm = (Phantoms.unTTerm inline)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Phantoms.unTTerm declaration)}]}))

templateDeclarationDeclaration :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm Syntax.Declaration
templateDeclarationDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
        Core.projectionField = (Core.Name "declaration")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateDeclarationInline :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm Bool
templateDeclarationInline x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
        Core.projectionField = (Core.Name "inline")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateDeclarationParameters :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm [String]
templateDeclarationParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateDeclarationWithDeclaration :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.TemplateDeclaration
templateDeclarationWithDeclaration original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inline"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
              Core.projectionField = (Core.Name "inline")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

templateDeclarationWithInline :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TemplateDeclaration
templateDeclarationWithInline original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inline"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
              Core.projectionField = (Core.Name "declaration")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateDeclarationWithParameters :: Phantoms.TTerm Syntax.TemplateDeclaration -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.TemplateDeclaration
templateDeclarationWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inline"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
              Core.projectionField = (Core.Name "inline")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateDeclaration"),
              Core.projectionField = (Core.Name "declaration")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateFunctionCallOperation :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm [Syntax.TemplateArgument] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.TemplateFunctionCallOperation
templateFunctionCallOperation function templateArguments arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "templateArguments"),
          Core.fieldTerm = (Phantoms.unTTerm templateArguments)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

templateFunctionCallOperationArguments :: Phantoms.TTerm Syntax.TemplateFunctionCallOperation -> Phantoms.TTerm [Syntax.Expression]
templateFunctionCallOperationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateFunctionCallOperationFunction :: Phantoms.TTerm Syntax.TemplateFunctionCallOperation -> Phantoms.TTerm Syntax.PostfixExpression
templateFunctionCallOperationFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateFunctionCallOperationTemplateArguments :: Phantoms.TTerm Syntax.TemplateFunctionCallOperation -> Phantoms.TTerm [Syntax.TemplateArgument]
templateFunctionCallOperationTemplateArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
        Core.projectionField = (Core.Name "templateArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateFunctionCallOperationWithArguments :: Phantoms.TTerm Syntax.TemplateFunctionCallOperation -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.TemplateFunctionCallOperation
templateFunctionCallOperationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templateArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
              Core.projectionField = (Core.Name "templateArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

templateFunctionCallOperationWithFunction :: Phantoms.TTerm Syntax.TemplateFunctionCallOperation -> Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.TemplateFunctionCallOperation
templateFunctionCallOperationWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "templateArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
              Core.projectionField = (Core.Name "templateArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateFunctionCallOperationWithTemplateArguments :: Phantoms.TTerm Syntax.TemplateFunctionCallOperation -> Phantoms.TTerm [Syntax.TemplateArgument] -> Phantoms.TTerm Syntax.TemplateFunctionCallOperation
templateFunctionCallOperationWithTemplateArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templateArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateFunctionCallOperation"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateType :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.TemplateArgument] -> Phantoms.TTerm Syntax.TemplateType
templateType name arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

templateTypeArguments :: Phantoms.TTerm Syntax.TemplateType -> Phantoms.TTerm [Syntax.TemplateArgument]
templateTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateType"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateTypeName :: Phantoms.TTerm Syntax.TemplateType -> Phantoms.TTerm String
templateTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateType"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateTypeWithArguments :: Phantoms.TTerm Syntax.TemplateType -> Phantoms.TTerm [Syntax.TemplateArgument] -> Phantoms.TTerm Syntax.TemplateType
templateTypeWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateType"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

templateTypeWithName :: Phantoms.TTerm Syntax.TemplateType -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TemplateType
templateTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TemplateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TemplateType"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ternaryExpression :: Phantoms.TTerm Syntax.LogicalOrExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.TernaryExpression
ternaryExpression condition trueExpr falseExpr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "trueExpr"),
          Core.fieldTerm = (Phantoms.unTTerm trueExpr)},
        Core.Field {
          Core.fieldName = (Core.Name "falseExpr"),
          Core.fieldTerm = (Phantoms.unTTerm falseExpr)}]}))

ternaryExpressionCondition :: Phantoms.TTerm Syntax.TernaryExpression -> Phantoms.TTerm Syntax.LogicalOrExpression
ternaryExpressionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ternaryExpressionFalseExpr :: Phantoms.TTerm Syntax.TernaryExpression -> Phantoms.TTerm Syntax.ConditionalExpression
ternaryExpressionFalseExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
        Core.projectionField = (Core.Name "falseExpr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ternaryExpressionTrueExpr :: Phantoms.TTerm Syntax.TernaryExpression -> Phantoms.TTerm Syntax.Expression
ternaryExpressionTrueExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
        Core.projectionField = (Core.Name "trueExpr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ternaryExpressionWithCondition :: Phantoms.TTerm Syntax.TernaryExpression -> Phantoms.TTerm Syntax.LogicalOrExpression -> Phantoms.TTerm Syntax.TernaryExpression
ternaryExpressionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trueExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
              Core.projectionField = (Core.Name "trueExpr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "falseExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
              Core.projectionField = (Core.Name "falseExpr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ternaryExpressionWithFalseExpr :: Phantoms.TTerm Syntax.TernaryExpression -> Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.TernaryExpression
ternaryExpressionWithFalseExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trueExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
              Core.projectionField = (Core.Name "trueExpr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "falseExpr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ternaryExpressionWithTrueExpr :: Phantoms.TTerm Syntax.TernaryExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TernaryExpression
ternaryExpressionWithTrueExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trueExpr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "falseExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TernaryExpression"),
              Core.projectionField = (Core.Name "falseExpr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeExpressionAuto :: Phantoms.TTerm Syntax.TypeExpression
typeExpressionAuto =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "auto"),
        Core.fieldTerm = Core.TermUnit}}))

typeExpressionBasic :: Phantoms.TTerm Syntax.BasicType -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionBasic x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionFunction :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionQualified :: Phantoms.TTerm Syntax.QualifiedType -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionQualified x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualified"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionTemplate :: Phantoms.TTerm Syntax.TemplateType -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionTemplate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeQualifierConst :: Phantoms.TTerm Syntax.TypeQualifier
typeQualifierConst =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = Core.TermUnit}}))

typeQualifierLvalueRef :: Phantoms.TTerm Syntax.TypeQualifier
typeQualifierLvalueRef =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lvalueRef"),
        Core.fieldTerm = Core.TermUnit}}))

typeQualifierPointer :: Phantoms.TTerm Syntax.TypeQualifier
typeQualifierPointer =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pointer"),
        Core.fieldTerm = Core.TermUnit}}))

typeQualifierRvalueRef :: Phantoms.TTerm Syntax.TypeQualifier
typeQualifierRvalueRef =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.TypeQualifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rvalueRef"),
        Core.fieldTerm = Core.TermUnit}}))

typedefDeclaration :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypedefDeclaration
typedefDeclaration name type_ isUsing =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "isUsing"),
          Core.fieldTerm = (Phantoms.unTTerm isUsing)}]}))

typedefDeclarationIsUsing :: Phantoms.TTerm Syntax.TypedefDeclaration -> Phantoms.TTerm Bool
typedefDeclarationIsUsing x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
        Core.projectionField = (Core.Name "isUsing")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedefDeclarationName :: Phantoms.TTerm Syntax.TypedefDeclaration -> Phantoms.TTerm String
typedefDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedefDeclarationType :: Phantoms.TTerm Syntax.TypedefDeclaration -> Phantoms.TTerm Syntax.TypeExpression
typedefDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedefDeclarationWithIsUsing :: Phantoms.TTerm Syntax.TypedefDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypedefDeclaration
typedefDeclarationWithIsUsing original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUsing"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typedefDeclarationWithName :: Phantoms.TTerm Syntax.TypedefDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TypedefDeclaration
typedefDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isUsing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
              Core.projectionField = (Core.Name "isUsing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typedefDeclarationWithType :: Phantoms.TTerm Syntax.TypedefDeclaration -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.TypedefDeclaration
typedefDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isUsing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.TypedefDeclaration"),
              Core.projectionField = (Core.Name "isUsing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unBooleanLiteral :: Phantoms.TTerm Syntax.BooleanLiteral -> Phantoms.TTerm Bool
unBooleanLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.BooleanLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unCharacterLiteral :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm String
unCharacterLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.CharacterLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unClassBody :: Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm [Syntax.MemberSpecification]
unClassBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.ClassBody")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unCompoundStatement :: Phantoms.TTerm Syntax.CompoundStatement -> Phantoms.TTerm [Syntax.Statement]
unCompoundStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.CompoundStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unExpressionStatement :: Phantoms.TTerm Syntax.ExpressionStatement -> Phantoms.TTerm Syntax.Expression
unExpressionStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.ExpressionStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFloatingLiteral :: Phantoms.TTerm Syntax.FloatingLiteral -> Phantoms.TTerm Double
unFloatingLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.FloatingLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unOverloadedLambdas :: Phantoms.TTerm Syntax.OverloadedLambdas -> Phantoms.TTerm [Syntax.LambdaExpression]
unOverloadedLambdas x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.OverloadedLambdas")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSizeofExpression :: Phantoms.TTerm Syntax.SizeofExpression -> Phantoms.TTerm Syntax.TypeExpression
unSizeofExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.SizeofExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStringLiteral :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm String
unStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.cpp.syntax.StringLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionPostfix :: Phantoms.TTerm Syntax.PostfixExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionPostfix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "postfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionSizeof :: Phantoms.TTerm Syntax.SizeofExpression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionSizeof x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sizeof"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryExpressionUnaryOp :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionUnaryOp x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unaryOp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryOperation :: Phantoms.TTerm Syntax.UnaryOperator -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryOperation
unaryOperation operator operand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)}]}))

unaryOperationOperand :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpression
unaryOperationOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperation"),
        Core.projectionField = (Core.Name "operand")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryOperationOperator :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryOperator
unaryOperationOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperation"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryOperationWithOperand :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryOperation
unaryOperationWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperation"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryOperationWithOperator :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryOperator -> Phantoms.TTerm Syntax.UnaryOperation
unaryOperationWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperation"),
              Core.projectionField = (Core.Name "operand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unaryOperatorAddressOf :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorAddressOf =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addressOf"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorBitwiseNot :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorBitwiseNot =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseNot"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorDereference :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorDereference =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dereference"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorLogicalNot :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorLogicalNot =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "logicalNot"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorMinus :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorMinus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorPlus :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorPlus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorPreDecrement :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorPreDecrement =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preDecrement"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorPreIncrement :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorPreIncrement =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preIncrement"),
        Core.fieldTerm = Core.TermUnit}}))

undefDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.UndefDirective
undefDirective name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.UndefDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefDirectiveName :: Phantoms.TTerm Syntax.UndefDirective -> Phantoms.TTerm String
undefDirectiveName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.UndefDirective"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefDirectiveWithName :: Phantoms.TTerm Syntax.UndefDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.UndefDirective
undefDirectiveWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.UndefDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableDeclaration :: Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm String -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclaration type_ name initializer isAuto =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Phantoms.unTTerm initializer)},
        Core.Field {
          Core.fieldName = (Core.Name "isAuto"),
          Core.fieldTerm = (Phantoms.unTTerm isAuto)}]}))

variableDeclarationInitializer :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm (Maybe Syntax.Expression)
variableDeclarationInitializer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
        Core.projectionField = (Core.Name "initializer")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclarationIsAuto :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Bool
variableDeclarationIsAuto x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
        Core.projectionField = (Core.Name "isAuto")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclarationName :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm String
variableDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclarationType :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm (Maybe Syntax.TypeExpression)
variableDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclarationWithInitializer :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclarationWithInitializer original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isAuto"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "isAuto")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDeclarationWithIsAuto :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclarationWithIsAuto original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "initializer")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isAuto"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableDeclarationWithName :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "initializer")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isAuto"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "isAuto")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDeclarationWithType :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "initializer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "initializer")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isAuto"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "isAuto")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variantDeclaration :: Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.VariantDeclaration
variantDeclaration types name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

variantDeclarationName :: Phantoms.TTerm Syntax.VariantDeclaration -> Phantoms.TTerm String
variantDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariantDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variantDeclarationTypes :: Phantoms.TTerm Syntax.VariantDeclaration -> Phantoms.TTerm [Syntax.TypeExpression]
variantDeclarationTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariantDeclaration"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variantDeclarationWithName :: Phantoms.TTerm Syntax.VariantDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.VariantDeclaration
variantDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariantDeclaration"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variantDeclarationWithTypes :: Phantoms.TTerm Syntax.VariantDeclaration -> Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm Syntax.VariantDeclaration
variantDeclarationWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.VariantDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.VariantDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vector :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Vector
vector elementType elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Vector"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm elementType)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

vectorElementType :: Phantoms.TTerm Syntax.Vector -> Phantoms.TTerm Syntax.TypeExpression
vectorElementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Vector"),
        Core.projectionField = (Core.Name "elementType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vectorElements :: Phantoms.TTerm Syntax.Vector -> Phantoms.TTerm [Syntax.Expression]
vectorElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Vector"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vectorWithElementType :: Phantoms.TTerm Syntax.Vector -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.Vector
vectorWithElementType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Vector"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Vector"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vectorWithElements :: Phantoms.TTerm Syntax.Vector -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Vector
vectorWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.Vector"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.Vector"),
              Core.projectionField = (Core.Name "elementType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

visitorLambda :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Visitor
visitorLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Visitor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

visitorOverloaded :: Phantoms.TTerm Syntax.OverloadedLambdas -> Phantoms.TTerm Syntax.Visitor
visitorOverloaded x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cpp.syntax.Visitor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "overloaded"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

warningDirective :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.WarningDirective
warningDirective message =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.WarningDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)}]}))

warningDirectiveMessage :: Phantoms.TTerm Syntax.WarningDirective -> Phantoms.TTerm String
warningDirectiveMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.WarningDirective"),
        Core.projectionField = (Core.Name "message")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

warningDirectiveWithMessage :: Phantoms.TTerm Syntax.WarningDirective -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.WarningDirective
warningDirectiveWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.WarningDirective"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

whileStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatement condition body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

whileStatementBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement
whileStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementCondition :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Expression
whileStatementCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementWithBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

whileStatementWithCondition :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
