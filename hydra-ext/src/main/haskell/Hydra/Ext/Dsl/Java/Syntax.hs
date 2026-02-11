-- | Meta-DSL for constructing Java syntax terms as first-class values.
-- Provides constructors and accessors for Java AST types,
-- replacing verbose inject/wrap/record/project calls with clean named functions.

module Hydra.Ext.Dsl.Java.Syntax where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Ext.Java.Syntax as Java

import Prelude hiding (map)


-- =============================================================================
-- Identifier (wrapper type)
-- =============================================================================

identifier :: TTerm String -> TTerm Java.Identifier
identifier = wrap Java._Identifier

unIdentifier :: TTerm Java.Identifier -> TTerm String
unIdentifier i = unwrap Java._Identifier @@ i

-- =============================================================================
-- TypeIdentifier (wrapper type, wraps Identifier)
-- =============================================================================

typeIdentifier :: TTerm Java.Identifier -> TTerm Java.TypeIdentifier
typeIdentifier = wrap Java._TypeIdentifier

unTypeIdentifier :: TTerm Java.TypeIdentifier -> TTerm Java.Identifier
unTypeIdentifier ti = unwrap Java._TypeIdentifier @@ ti

-- =============================================================================
-- IntegerLiteral (wrapper type)
-- =============================================================================

integerLiteral :: TTerm Integer -> TTerm Java.IntegerLiteral
integerLiteral = wrap Java._IntegerLiteral

-- =============================================================================
-- FloatingPointLiteral (wrapper type)
-- =============================================================================

floatingPointLiteral :: TTerm Double -> TTerm Java.FloatingPointLiteral
floatingPointLiteral = wrap Java._FloatingPointLiteral

-- =============================================================================
-- StringLiteral (wrapper type)
-- =============================================================================

stringLiteral :: TTerm String -> TTerm Java.StringLiteral
stringLiteral = wrap Java._StringLiteral

-- =============================================================================
-- Literal (union type)
-- =============================================================================

literalNull :: TTerm Java.Literal
literalNull = injectUnit Java._Literal Java._Literal_null

literalInteger :: TTerm Java.IntegerLiteral -> TTerm Java.Literal
literalInteger = inject Java._Literal Java._Literal_integer

literalFloatingPoint :: TTerm Java.FloatingPointLiteral -> TTerm Java.Literal
literalFloatingPoint = inject Java._Literal Java._Literal_floatingPoint

literalBoolean :: TTerm Bool -> TTerm Java.Literal
literalBoolean = inject Java._Literal Java._Literal_boolean

literalCharacter :: TTerm Int -> TTerm Java.Literal
literalCharacter = inject Java._Literal Java._Literal_character

literalString :: TTerm Java.StringLiteral -> TTerm Java.Literal
literalString = inject Java._Literal Java._Literal_string

-- =============================================================================
-- Type (union type)
-- =============================================================================

typePrimitive :: TTerm Java.PrimitiveTypeWithAnnotations -> TTerm Java.Type
typePrimitive = inject Java._Type Java._Type_primitive

typeReference :: TTerm Java.ReferenceType -> TTerm Java.Type
typeReference = inject Java._Type Java._Type_reference

-- =============================================================================
-- PrimitiveTypeWithAnnotations (record type)
-- =============================================================================

primitiveTypeWithAnnotations :: TTerm Java.PrimitiveType -> TTerm [Java.Annotation] -> TTerm Java.PrimitiveTypeWithAnnotations
primitiveTypeWithAnnotations t anns = record Java._PrimitiveTypeWithAnnotations [
  Java._PrimitiveTypeWithAnnotations_type>>: t,
  Java._PrimitiveTypeWithAnnotations_annotations>>: anns]

-- =============================================================================
-- PrimitiveType (union type)
-- =============================================================================

primitiveTypeNumeric :: TTerm Java.NumericType -> TTerm Java.PrimitiveType
primitiveTypeNumeric = inject Java._PrimitiveType Java._PrimitiveType_numeric

primitiveTypeBoolean :: TTerm Java.PrimitiveType
primitiveTypeBoolean = injectUnit Java._PrimitiveType Java._PrimitiveType_boolean

-- =============================================================================
-- NumericType (union type)
-- =============================================================================

numericTypeIntegral :: TTerm Java.IntegralType -> TTerm Java.NumericType
numericTypeIntegral = inject Java._NumericType Java._NumericType_integral

numericTypeFloatingPoint :: TTerm Java.FloatingPointType -> TTerm Java.NumericType
numericTypeFloatingPoint = inject Java._NumericType Java._NumericType_floatingPoint

-- =============================================================================
-- IntegralType (enum-like union)
-- =============================================================================

integralTypeByte :: TTerm Java.IntegralType
integralTypeByte = injectUnit Java._IntegralType Java._IntegralType_byte

integralTypeShort :: TTerm Java.IntegralType
integralTypeShort = injectUnit Java._IntegralType Java._IntegralType_short

integralTypeInt :: TTerm Java.IntegralType
integralTypeInt = injectUnit Java._IntegralType Java._IntegralType_int

integralTypeLong :: TTerm Java.IntegralType
integralTypeLong = injectUnit Java._IntegralType Java._IntegralType_long

integralTypeChar :: TTerm Java.IntegralType
integralTypeChar = injectUnit Java._IntegralType Java._IntegralType_char

-- =============================================================================
-- FloatingPointType (enum-like union)
-- =============================================================================

floatingPointTypeFloat :: TTerm Java.FloatingPointType
floatingPointTypeFloat = injectUnit Java._FloatingPointType Java._FloatingPointType_float

floatingPointTypeDouble :: TTerm Java.FloatingPointType
floatingPointTypeDouble = injectUnit Java._FloatingPointType Java._FloatingPointType_double

-- =============================================================================
-- ReferenceType (union type)
-- =============================================================================

referenceTypeClassOrInterface :: TTerm Java.ClassOrInterfaceType -> TTerm Java.ReferenceType
referenceTypeClassOrInterface = inject Java._ReferenceType Java._ReferenceType_classOrInterface

referenceTypeVariable :: TTerm Java.TypeVariable -> TTerm Java.ReferenceType
referenceTypeVariable = inject Java._ReferenceType Java._ReferenceType_variable

referenceTypeArray :: TTerm Java.ArrayType -> TTerm Java.ReferenceType
referenceTypeArray = inject Java._ReferenceType Java._ReferenceType_array

-- =============================================================================
-- ClassOrInterfaceType (union type)
-- =============================================================================

classOrInterfaceTypeClass :: TTerm Java.ClassType -> TTerm Java.ClassOrInterfaceType
classOrInterfaceTypeClass = inject Java._ClassOrInterfaceType Java._ClassOrInterfaceType_class

classOrInterfaceTypeInterface :: TTerm Java.InterfaceType -> TTerm Java.ClassOrInterfaceType
classOrInterfaceTypeInterface = inject Java._ClassOrInterfaceType Java._ClassOrInterfaceType_interface

-- =============================================================================
-- ClassType (record type)
-- =============================================================================

classType :: TTerm [Java.Annotation] -> TTerm Java.ClassTypeQualifier -> TTerm Java.TypeIdentifier -> TTerm [Java.TypeArgument] -> TTerm Java.ClassType
classType anns qual ident args = record Java._ClassType [
  Java._ClassType_annotations>>: anns,
  Java._ClassType_qualifier>>: qual,
  Java._ClassType_identifier>>: ident,
  Java._ClassType_arguments>>: args]

-- =============================================================================
-- ClassTypeQualifier (union type)
-- =============================================================================

classTypeQualifierNone :: TTerm Java.ClassTypeQualifier
classTypeQualifierNone = injectUnit Java._ClassTypeQualifier Java._ClassTypeQualifier_none

classTypeQualifierPackage :: TTerm Java.PackageName -> TTerm Java.ClassTypeQualifier
classTypeQualifierPackage = inject Java._ClassTypeQualifier Java._ClassTypeQualifier_package

classTypeQualifierParent :: TTerm Java.ClassOrInterfaceType -> TTerm Java.ClassTypeQualifier
classTypeQualifierParent = inject Java._ClassTypeQualifier Java._ClassTypeQualifier_parent

-- =============================================================================
-- InterfaceType (wrapper type)
-- =============================================================================

interfaceType :: TTerm Java.ClassType -> TTerm Java.InterfaceType
interfaceType = wrap Java._InterfaceType

-- =============================================================================
-- TypeVariable (record type)
-- =============================================================================

typeVariable :: TTerm [Java.Annotation] -> TTerm Java.TypeIdentifier -> TTerm Java.TypeVariable
typeVariable anns ident = record Java._TypeVariable [
  Java._TypeVariable_annotations>>: anns,
  Java._TypeVariable_identifier>>: ident]

-- =============================================================================
-- ArrayType (record type)
-- =============================================================================

arrayType :: TTerm Java.Dims -> TTerm Java.ArrayType_Variant -> TTerm Java.ArrayType
arrayType dims variant = record Java._ArrayType [
  Java._ArrayType_dims>>: dims,
  Java._ArrayType_variant>>: variant]

arrayTypeVariantPrimitive :: TTerm Java.PrimitiveTypeWithAnnotations -> TTerm Java.ArrayType_Variant
arrayTypeVariantPrimitive = inject Java._ArrayType_Variant Java._ArrayType_Variant_primitive

-- =============================================================================
-- Dims (wrapper type)
-- =============================================================================

dims :: TTerm [[Java.Annotation]] -> TTerm Java.Dims
dims = wrap Java._Dims

-- =============================================================================
-- TypeParameter (record type)
-- =============================================================================

typeParameter :: TTerm [Java.TypeParameterModifier] -> TTerm Java.TypeIdentifier -> TTerm (Maybe Java.TypeBound) -> TTerm Java.TypeParameter
typeParameter mods ident bound = record Java._TypeParameter [
  Java._TypeParameter_modifiers>>: mods,
  Java._TypeParameter_identifier>>: ident,
  Java._TypeParameter_bound>>: bound]

typeParameterIdentifier :: TTerm Java.TypeParameter -> TTerm Java.TypeIdentifier
typeParameterIdentifier tp = project Java._TypeParameter Java._TypeParameter_identifier @@ tp

-- =============================================================================
-- TypeArgument (union type)
-- =============================================================================

typeArgumentReference :: TTerm Java.ReferenceType -> TTerm Java.TypeArgument
typeArgumentReference = inject Java._TypeArgument Java._TypeArgument_reference

typeArgumentWildcard :: TTerm Java.Wildcard -> TTerm Java.TypeArgument
typeArgumentWildcard = inject Java._TypeArgument Java._TypeArgument_wildcard

-- =============================================================================
-- Wildcard (record type)
-- =============================================================================

wildcard :: TTerm [Java.Annotation] -> TTerm (Maybe Java.WildcardBounds) -> TTerm Java.Wildcard
wildcard anns bounds = record Java._Wildcard [
  Java._Wildcard_annotations>>: anns,
  Java._Wildcard_wildcard>>: bounds]

-- =============================================================================
-- PackageName (wrapper type)
-- =============================================================================

packageName :: TTerm [Java.Identifier] -> TTerm Java.PackageName
packageName = wrap Java._PackageName

-- =============================================================================
-- AmbiguousName (wrapper type)
-- =============================================================================

ambiguousName :: TTerm [Java.Identifier] -> TTerm Java.AmbiguousName
ambiguousName = wrap Java._AmbiguousName

-- =============================================================================
-- MethodName (wrapper type)
-- =============================================================================

methodName :: TTerm Java.Identifier -> TTerm Java.MethodName
methodName = wrap Java._MethodName

-- =============================================================================
-- TypeName (record type)
-- =============================================================================

typeName :: TTerm Java.TypeIdentifier -> TTerm (Maybe Java.PackageOrTypeName) -> TTerm Java.TypeName
typeName ident qual = record Java._TypeName [
  Java._TypeName_identifier>>: ident,
  Java._TypeName_qualifier>>: qual]

-- =============================================================================
-- ExpressionName (record type)
-- =============================================================================

expressionName :: TTerm (Maybe Java.AmbiguousName) -> TTerm Java.Identifier -> TTerm Java.ExpressionName
expressionName qual ident = record Java._ExpressionName [
  Java._ExpressionName_qualifier>>: qual,
  Java._ExpressionName_identifier>>: ident]

-- =============================================================================
-- Block (wrapper type)
-- =============================================================================

block :: TTerm [Java.BlockStatement] -> TTerm Java.Block
block = wrap Java._Block

-- =============================================================================
-- BlockStatement (union type)
-- =============================================================================

blockStatementLocalVariableDeclaration :: TTerm Java.LocalVariableDeclarationStatement -> TTerm Java.BlockStatement
blockStatementLocalVariableDeclaration = inject Java._BlockStatement Java._BlockStatement_localVariableDeclaration

blockStatementStatement :: TTerm Java.Statement -> TTerm Java.BlockStatement
blockStatementStatement = inject Java._BlockStatement Java._BlockStatement_statement

blockStatementClass :: TTerm Java.ClassDeclaration -> TTerm Java.BlockStatement
blockStatementClass = inject Java._BlockStatement Java._BlockStatement_class

-- =============================================================================
-- LocalVariableDeclarationStatement (wrapper type)
-- =============================================================================

localVariableDeclarationStatement :: TTerm Java.LocalVariableDeclaration -> TTerm Java.LocalVariableDeclarationStatement
localVariableDeclarationStatement = wrap Java._LocalVariableDeclarationStatement

-- =============================================================================
-- LocalVariableDeclaration (record type)
-- =============================================================================

localVariableDeclaration :: TTerm [Java.VariableModifier] -> TTerm Java.LocalVariableType -> TTerm [Java.VariableDeclarator] -> TTerm Java.LocalVariableDeclaration
localVariableDeclaration mods lvt vars = record Java._LocalVariableDeclaration [
  Java._LocalVariableDeclaration_modifiers>>: mods,
  Java._LocalVariableDeclaration_type>>: lvt,
  Java._LocalVariableDeclaration_declarators>>: vars]

-- =============================================================================
-- LocalVariableType (union type)
-- =============================================================================

localVariableTypeType :: TTerm Java.UnannType -> TTerm Java.LocalVariableType
localVariableTypeType = inject Java._LocalVariableType Java._LocalVariableType_type

localVariableTypeVar :: TTerm Java.LocalVariableType
localVariableTypeVar = injectUnit Java._LocalVariableType Java._LocalVariableType_var

-- =============================================================================
-- UnannType (wrapper type)
-- =============================================================================

unannType :: TTerm Java.Type -> TTerm Java.UnannType
unannType = wrap Java._UnannType

-- =============================================================================
-- Statement (union type)
-- =============================================================================

statementWithoutTrailing :: TTerm Java.StatementWithoutTrailingSubstatement -> TTerm Java.Statement
statementWithoutTrailing = inject Java._Statement Java._Statement_withoutTrailing

statementLabeled :: TTerm Java.LabeledStatement -> TTerm Java.Statement
statementLabeled = inject Java._Statement Java._Statement_labeled

statementIfThen :: TTerm Java.IfThenStatement -> TTerm Java.Statement
statementIfThen = inject Java._Statement Java._Statement_ifThen

statementIfThenElse :: TTerm Java.IfThenElseStatement -> TTerm Java.Statement
statementIfThenElse = inject Java._Statement Java._Statement_ifThenElse

statementWhile :: TTerm Java.WhileStatement -> TTerm Java.Statement
statementWhile = inject Java._Statement Java._Statement_while

statementFor :: TTerm Java.ForStatement -> TTerm Java.Statement
statementFor = inject Java._Statement Java._Statement_for

-- =============================================================================
-- StatementWithoutTrailingSubstatement (union type)
-- =============================================================================

stmtExpression :: TTerm Java.ExpressionStatement -> TTerm Java.StatementWithoutTrailingSubstatement
stmtExpression = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_expression

stmtReturn :: TTerm Java.ReturnStatement -> TTerm Java.StatementWithoutTrailingSubstatement
stmtReturn = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_return

stmtThrow :: TTerm Java.ThrowStatement -> TTerm Java.StatementWithoutTrailingSubstatement
stmtThrow = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_throw

stmtEmpty :: TTerm Java.StatementWithoutTrailingSubstatement
stmtEmpty = injectUnit Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_empty

stmtBlock :: TTerm Java.Block -> TTerm Java.StatementWithoutTrailingSubstatement
stmtBlock = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_block

-- =============================================================================
-- ExpressionStatement (wrapper type)
-- =============================================================================

expressionStatement :: TTerm Java.StatementExpression -> TTerm Java.ExpressionStatement
expressionStatement = wrap Java._ExpressionStatement

-- =============================================================================
-- ReturnStatement (wrapper type)
-- =============================================================================

returnStatement :: TTerm (Maybe Java.Expression) -> TTerm Java.ReturnStatement
returnStatement = wrap Java._ReturnStatement

-- =============================================================================
-- ThrowStatement (wrapper type)
-- =============================================================================

throwStatement :: TTerm Java.Expression -> TTerm Java.ThrowStatement
throwStatement = wrap Java._ThrowStatement

-- =============================================================================
-- StatementExpression (union type)
-- =============================================================================

stmtExprAssignment :: TTerm Java.Assignment -> TTerm Java.StatementExpression
stmtExprAssignment = inject Java._StatementExpression Java._StatementExpression_assignment

stmtExprMethodInvocation :: TTerm Java.MethodInvocation -> TTerm Java.StatementExpression
stmtExprMethodInvocation = inject Java._StatementExpression Java._StatementExpression_methodInvocation

stmtExprClassInstance :: TTerm Java.ClassInstanceCreationExpression -> TTerm Java.StatementExpression
stmtExprClassInstance = inject Java._StatementExpression Java._StatementExpression_classInstanceCreation

-- =============================================================================
-- IfThenStatement (record type)
-- =============================================================================

ifThenStatement :: TTerm Java.Expression -> TTerm Java.Statement -> TTerm Java.IfThenStatement
ifThenStatement cond stmt = record Java._IfThenStatement [
  Java._IfThenStatement_expression>>: cond,
  Java._IfThenStatement_statement>>: stmt]

-- =============================================================================
-- Assignment (record type)
-- =============================================================================

assignment :: TTerm Java.LeftHandSide -> TTerm Java.AssignmentOperator -> TTerm Java.Expression -> TTerm Java.Assignment
assignment lhs op expr = record Java._Assignment [
  Java._Assignment_lhs>>: lhs,
  Java._Assignment_op>>: op,
  Java._Assignment_expression>>: expr]

-- =============================================================================
-- AssignmentOperator (union type)
-- =============================================================================

assignmentOperatorSimple :: TTerm Java.AssignmentOperator
assignmentOperatorSimple = injectUnit Java._AssignmentOperator Java._AssignmentOperator_simple

-- =============================================================================
-- LeftHandSide (union type)
-- =============================================================================

leftHandSideExpressionName :: TTerm Java.ExpressionName -> TTerm Java.LeftHandSide
leftHandSideExpressionName = inject Java._LeftHandSide Java._LeftHandSide_expressionName

-- =============================================================================
-- Expression (union type)
-- =============================================================================

expressionAssignment :: TTerm Java.AssignmentExpression -> TTerm Java.Expression
expressionAssignment = inject Java._Expression Java._Expression_assignment

expressionLambda :: TTerm Java.LambdaExpression -> TTerm Java.Expression
expressionLambda = inject Java._Expression Java._Expression_lambda

-- =============================================================================
-- AssignmentExpression (union type)
-- =============================================================================

assignmentExpressionConditional :: TTerm Java.ConditionalExpression -> TTerm Java.AssignmentExpression
assignmentExpressionConditional = inject Java._AssignmentExpression Java._AssignmentExpression_conditional

assignmentExpressionAssignment :: TTerm Java.Assignment -> TTerm Java.AssignmentExpression
assignmentExpressionAssignment = inject Java._AssignmentExpression Java._AssignmentExpression_assignment

-- =============================================================================
-- ConditionalExpression (union type)
-- =============================================================================

conditionalExpressionSimple :: TTerm Java.ConditionalOrExpression -> TTerm Java.ConditionalExpression
conditionalExpressionSimple = inject Java._ConditionalExpression Java._ConditionalExpression_simple

-- =============================================================================
-- ConditionalOrExpression, ConditionalAndExpression, InclusiveOrExpression,
-- ExclusiveOrExpression, AndExpression (wrapper types wrapping lists)
-- =============================================================================

conditionalOrExpression :: TTerm [Java.ConditionalAndExpression] -> TTerm Java.ConditionalOrExpression
conditionalOrExpression = wrap Java._ConditionalOrExpression

conditionalAndExpression :: TTerm [Java.InclusiveOrExpression] -> TTerm Java.ConditionalAndExpression
conditionalAndExpression = wrap Java._ConditionalAndExpression

inclusiveOrExpression :: TTerm [Java.ExclusiveOrExpression] -> TTerm Java.InclusiveOrExpression
inclusiveOrExpression = wrap Java._InclusiveOrExpression

exclusiveOrExpression :: TTerm [Java.AndExpression] -> TTerm Java.ExclusiveOrExpression
exclusiveOrExpression = wrap Java._ExclusiveOrExpression

andExpression :: TTerm [Java.EqualityExpression] -> TTerm Java.AndExpression
andExpression = wrap Java._AndExpression

-- =============================================================================
-- EqualityExpression (union type)
-- =============================================================================

equalityExpressionUnary :: TTerm Java.RelationalExpression -> TTerm Java.EqualityExpression
equalityExpressionUnary = inject Java._EqualityExpression Java._EqualityExpression_unary

equalityExpressionEqual :: TTerm Java.EqualityExpression_Binary -> TTerm Java.EqualityExpression
equalityExpressionEqual = inject Java._EqualityExpression Java._EqualityExpression_equal

equalityExpressionNotEqual :: TTerm Java.EqualityExpression_Binary -> TTerm Java.EqualityExpression
equalityExpressionNotEqual = inject Java._EqualityExpression Java._EqualityExpression_notEqual

equalityExpressionBinary :: TTerm Java.EqualityExpression -> TTerm Java.RelationalExpression -> TTerm Java.EqualityExpression_Binary
equalityExpressionBinary lhs rhs = record Java._EqualityExpression_Binary [
  Java._EqualityExpression_Binary_lhs>>: lhs,
  Java._EqualityExpression_Binary_rhs>>: rhs]

-- =============================================================================
-- RelationalExpression (union type)
-- =============================================================================

relationalExpressionSimple :: TTerm Java.ShiftExpression -> TTerm Java.RelationalExpression
relationalExpressionSimple = inject Java._RelationalExpression Java._RelationalExpression_simple

relationalExpressionInstanceOf :: TTerm Java.RelationalExpression_InstanceOf -> TTerm Java.RelationalExpression
relationalExpressionInstanceOf = inject Java._RelationalExpression Java._RelationalExpression_instanceof

relationalExpressionInstanceOf_ :: TTerm Java.RelationalExpression -> TTerm Java.ReferenceType -> TTerm Java.RelationalExpression_InstanceOf
relationalExpressionInstanceOf_ expr rt = record Java._RelationalExpression_InstanceOf [
  Java._RelationalExpression_InstanceOf_lhs>>: expr,
  Java._RelationalExpression_InstanceOf_rhs>>: rt]

-- =============================================================================
-- ShiftExpression (union type)
-- =============================================================================

shiftExpressionUnary :: TTerm Java.AdditiveExpression -> TTerm Java.ShiftExpression
shiftExpressionUnary = inject Java._ShiftExpression Java._ShiftExpression_unary

-- =============================================================================
-- AdditiveExpression (union type)
-- =============================================================================

additiveExpressionUnary :: TTerm Java.MultiplicativeExpression -> TTerm Java.AdditiveExpression
additiveExpressionUnary = inject Java._AdditiveExpression Java._AdditiveExpression_unary

additiveExpressionPlus :: TTerm Java.AdditiveExpression_Binary -> TTerm Java.AdditiveExpression
additiveExpressionPlus = inject Java._AdditiveExpression Java._AdditiveExpression_plus

additiveExpressionBinary :: TTerm Java.AdditiveExpression -> TTerm Java.MultiplicativeExpression -> TTerm Java.AdditiveExpression_Binary
additiveExpressionBinary lhs rhs = record Java._AdditiveExpression_Binary [
  Java._AdditiveExpression_Binary_lhs>>: lhs,
  Java._AdditiveExpression_Binary_rhs>>: rhs]

-- =============================================================================
-- MultiplicativeExpression (union type)
-- =============================================================================

multiplicativeExpressionUnary :: TTerm Java.UnaryExpression -> TTerm Java.MultiplicativeExpression
multiplicativeExpressionUnary = inject Java._MultiplicativeExpression Java._MultiplicativeExpression_unary

multiplicativeExpressionTimes :: TTerm Java.MultiplicativeExpression_Binary -> TTerm Java.MultiplicativeExpression
multiplicativeExpressionTimes = inject Java._MultiplicativeExpression Java._MultiplicativeExpression_times

multiplicativeExpressionBinary :: TTerm Java.MultiplicativeExpression -> TTerm Java.UnaryExpression -> TTerm Java.MultiplicativeExpression_Binary
multiplicativeExpressionBinary lhs rhs = record Java._MultiplicativeExpression_Binary [
  Java._MultiplicativeExpression_Binary_lhs>>: lhs,
  Java._MultiplicativeExpression_Binary_rhs>>: rhs]

-- =============================================================================
-- UnaryExpression (union type)
-- =============================================================================

unaryExpressionOther :: TTerm Java.UnaryExpressionNotPlusMinus -> TTerm Java.UnaryExpression
unaryExpressionOther = inject Java._UnaryExpression Java._UnaryExpression_other

unaryExpressionNotPlusMinusPostfix :: TTerm Java.PostfixExpression -> TTerm Java.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusPostfix = inject Java._UnaryExpressionNotPlusMinus Java._UnaryExpressionNotPlusMinus_postfix

unaryExpressionNotPlusMinusCast :: TTerm Java.CastExpression -> TTerm Java.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusCast = inject Java._UnaryExpressionNotPlusMinus Java._UnaryExpressionNotPlusMinus_cast

unaryExpressionNotPlusMinusNot :: TTerm Java.UnaryExpression -> TTerm Java.UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinusNot = inject Java._UnaryExpressionNotPlusMinus Java._UnaryExpressionNotPlusMinus_not

-- =============================================================================
-- PostfixExpression (union type)
-- =============================================================================

postfixExpressionPrimary :: TTerm Java.Primary -> TTerm Java.PostfixExpression
postfixExpressionPrimary = inject Java._PostfixExpression Java._PostfixExpression_primary

postfixExpressionName :: TTerm Java.ExpressionName -> TTerm Java.PostfixExpression
postfixExpressionName = inject Java._PostfixExpression Java._PostfixExpression_name

-- =============================================================================
-- Primary (union type)
-- =============================================================================

primaryNoNewArray :: TTerm Java.PrimaryNoNewArray -> TTerm Java.Primary
primaryNoNewArray = inject Java._Primary Java._Primary_noNewArray

primaryArrayCreation :: TTerm Java.ArrayCreationExpression -> TTerm Java.Primary
primaryArrayCreation = inject Java._Primary Java._Primary_arrayCreation

-- =============================================================================
-- PrimaryNoNewArray (union type)
-- =============================================================================

primaryLiteral :: TTerm Java.Literal -> TTerm Java.PrimaryNoNewArray
primaryLiteral = inject Java._PrimaryNoNewArray Java._PrimaryNoNewArray_literal

primaryParens :: TTerm Java.Expression -> TTerm Java.PrimaryNoNewArray
primaryParens = inject Java._PrimaryNoNewArray Java._PrimaryNoNewArray_parens

primaryFieldAccess :: TTerm Java.FieldAccess -> TTerm Java.PrimaryNoNewArray
primaryFieldAccess = inject Java._PrimaryNoNewArray Java._PrimaryNoNewArray_fieldAccess

primaryMethodInvocation :: TTerm Java.MethodInvocation -> TTerm Java.PrimaryNoNewArray
primaryMethodInvocation = inject Java._PrimaryNoNewArray Java._PrimaryNoNewArray_methodInvocation

primaryThis :: TTerm Java.PrimaryNoNewArray
primaryThis = injectUnit Java._PrimaryNoNewArray Java._PrimaryNoNewArray_this

primaryClassInstance :: TTerm Java.ClassInstanceCreationExpression -> TTerm Java.PrimaryNoNewArray
primaryClassInstance = inject Java._PrimaryNoNewArray Java._PrimaryNoNewArray_classInstance

primaryArrayAccess :: TTerm Java.ArrayAccess -> TTerm Java.PrimaryNoNewArray
primaryArrayAccess = inject Java._PrimaryNoNewArray Java._PrimaryNoNewArray_arrayAccess

-- =============================================================================
-- CastExpression (union type)
-- =============================================================================

castExpressionNotPlusMinus :: TTerm Java.CastExpression_NotPlusMinus -> TTerm Java.CastExpression
castExpressionNotPlusMinus = inject Java._CastExpression Java._CastExpression_notPlusMinus

castExpressionPrimitive :: TTerm Java.CastExpression_Primitive -> TTerm Java.CastExpression
castExpressionPrimitive = inject Java._CastExpression Java._CastExpression_primitive

castExpressionNotPlusMinus_ :: TTerm Java.CastExpression_RefAndBounds -> TTerm Java.UnaryExpressionNotPlusMinus -> TTerm Java.CastExpression_NotPlusMinus
castExpressionNotPlusMinus_ rb expr = record Java._CastExpression_NotPlusMinus [
  Java._CastExpression_NotPlusMinus_refAndBounds>>: rb,
  Java._CastExpression_NotPlusMinus_expression>>: expr]

castExpressionRefAndBounds :: TTerm Java.ReferenceType -> TTerm [Java.AdditionalBound] -> TTerm Java.CastExpression_RefAndBounds
castExpressionRefAndBounds rt bounds = record Java._CastExpression_RefAndBounds [
  Java._CastExpression_RefAndBounds_type>>: rt,
  Java._CastExpression_RefAndBounds_bounds>>: bounds]

castExpressionPrimitive_ :: TTerm Java.PrimitiveTypeWithAnnotations -> TTerm Java.UnaryExpression -> TTerm Java.CastExpression_Primitive
castExpressionPrimitive_ pt expr = record Java._CastExpression_Primitive [
  Java._CastExpression_Primitive_type>>: pt,
  Java._CastExpression_Primitive_expression>>: expr]

-- =============================================================================
-- FieldAccess (record type)
-- =============================================================================

fieldAccess :: TTerm Java.FieldAccess_Qualifier -> TTerm Java.Identifier -> TTerm Java.FieldAccess
fieldAccess qual ident = record Java._FieldAccess [
  Java._FieldAccess_qualifier>>: qual,
  Java._FieldAccess_identifier>>: ident]

fieldAccessQualifierPrimary :: TTerm Java.Primary -> TTerm Java.FieldAccess_Qualifier
fieldAccessQualifierPrimary = inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary

-- =============================================================================
-- MethodInvocation (record type)
-- =============================================================================

methodInvocation_ :: TTerm Java.MethodInvocation_Header -> TTerm [Java.Expression] -> TTerm Java.MethodInvocation
methodInvocation_ header args = record Java._MethodInvocation [
  Java._MethodInvocation_header>>: header,
  Java._MethodInvocation_arguments>>: args]

-- =============================================================================
-- MethodInvocation_Header (union type)
-- =============================================================================

methodInvocationHeaderSimple :: TTerm Java.MethodName -> TTerm Java.MethodInvocation_Header
methodInvocationHeaderSimple = inject Java._MethodInvocation_Header Java._MethodInvocation_Header_simple

methodInvocationHeaderComplex :: TTerm Java.MethodInvocation_Complex -> TTerm Java.MethodInvocation_Header
methodInvocationHeaderComplex = inject Java._MethodInvocation_Header Java._MethodInvocation_Header_complex

-- =============================================================================
-- MethodInvocation_Complex (record type)
-- =============================================================================

methodInvocationComplex :: TTerm Java.MethodInvocation_Variant -> TTerm [Java.TypeArgument] -> TTerm Java.Identifier -> TTerm Java.MethodInvocation_Complex
methodInvocationComplex variant targs ident = record Java._MethodInvocation_Complex [
  Java._MethodInvocation_Complex_variant>>: variant,
  Java._MethodInvocation_Complex_typeArguments>>: targs,
  Java._MethodInvocation_Complex_identifier>>: ident]

-- =============================================================================
-- MethodInvocation_Variant (union type)
-- =============================================================================

methodInvocationVariantExpression :: TTerm Java.ExpressionName -> TTerm Java.MethodInvocation_Variant
methodInvocationVariantExpression = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_expression

methodInvocationVariantPrimary :: TTerm Java.Primary -> TTerm Java.MethodInvocation_Variant
methodInvocationVariantPrimary = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_primary

methodInvocationVariantType :: TTerm Java.TypeName -> TTerm Java.MethodInvocation_Variant
methodInvocationVariantType = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_type

-- =============================================================================
-- LambdaExpression (record type)
-- =============================================================================

lambdaExpression :: TTerm Java.LambdaParameters -> TTerm Java.LambdaBody -> TTerm Java.LambdaExpression
lambdaExpression params body = record Java._LambdaExpression [
  Java._LambdaExpression_parameters>>: params,
  Java._LambdaExpression_body>>: body]

-- =============================================================================
-- LambdaParameters (union type)
-- =============================================================================

lambdaParametersSingle :: TTerm Java.Identifier -> TTerm Java.LambdaParameters
lambdaParametersSingle = inject Java._LambdaParameters Java._LambdaParameters_single

lambdaParametersTuple :: TTerm [Java.FormalParameter] -> TTerm Java.LambdaParameters
lambdaParametersTuple = inject Java._LambdaParameters Java._LambdaParameters_tuple

-- =============================================================================
-- LambdaBody (union type)
-- =============================================================================

lambdaBodyExpression :: TTerm Java.Expression -> TTerm Java.LambdaBody
lambdaBodyExpression = inject Java._LambdaBody Java._LambdaBody_expression

lambdaBodyBlock :: TTerm Java.Block -> TTerm Java.LambdaBody
lambdaBodyBlock = inject Java._LambdaBody Java._LambdaBody_block

-- =============================================================================
-- ClassInstanceCreationExpression (record type)
-- =============================================================================

classInstanceCreationExpression :: TTerm (Maybe Java.Expression) -> TTerm Java.UnqualifiedClassInstanceCreationExpression -> TTerm Java.ClassInstanceCreationExpression
classInstanceCreationExpression qual unqual = record Java._ClassInstanceCreationExpression [
  Java._ClassInstanceCreationExpression_qualifier>>: qual,
  Java._ClassInstanceCreationExpression_expression>>: unqual]

unqualifiedClassInstanceCreationExpression :: TTerm [Java.TypeArgument] -> TTerm Java.ClassOrInterfaceTypeToInstantiate -> TTerm [Java.Expression] -> TTerm (Maybe Java.ClassBody) -> TTerm Java.UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression targs ci args body = record Java._UnqualifiedClassInstanceCreationExpression [
  Java._UnqualifiedClassInstanceCreationExpression_typeArguments>>: targs,
  Java._UnqualifiedClassInstanceCreationExpression_classOrInterface>>: ci,
  Java._UnqualifiedClassInstanceCreationExpression_arguments>>: args,
  Java._UnqualifiedClassInstanceCreationExpression_body>>: body]

classOrInterfaceTypeToInstantiate :: TTerm [Java.AnnotatedIdentifier] -> TTerm (Maybe Java.TypeArgumentsOrDiamond) -> TTerm Java.ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate ids targs = record Java._ClassOrInterfaceTypeToInstantiate [
  Java._ClassOrInterfaceTypeToInstantiate_identifiers>>: ids,
  Java._ClassOrInterfaceTypeToInstantiate_typeArguments>>: targs]

annotatedIdentifier :: TTerm [Java.Annotation] -> TTerm Java.Identifier -> TTerm Java.AnnotatedIdentifier
annotatedIdentifier anns ident = record Java._AnnotatedIdentifier [
  Java._AnnotatedIdentifier_annotations>>: anns,
  Java._AnnotatedIdentifier_identifier>>: ident]

-- =============================================================================
-- TypeArgumentsOrDiamond (union type)
-- =============================================================================

typeArgumentsOrDiamondDiamond :: TTerm Java.TypeArgumentsOrDiamond
typeArgumentsOrDiamondDiamond = injectUnit Java._TypeArgumentsOrDiamond Java._TypeArgumentsOrDiamond_diamond

typeArgumentsOrDiamondArguments :: TTerm [Java.TypeArgument] -> TTerm Java.TypeArgumentsOrDiamond
typeArgumentsOrDiamondArguments = inject Java._TypeArgumentsOrDiamond Java._TypeArgumentsOrDiamond_arguments

-- =============================================================================
-- VariableDeclarator (record type)
-- =============================================================================

variableDeclarator :: TTerm Java.VariableDeclaratorId -> TTerm (Maybe Java.VariableInitializer) -> TTerm Java.VariableDeclarator
variableDeclarator vid init_ = record Java._VariableDeclarator [
  Java._VariableDeclarator_id>>: vid,
  Java._VariableDeclarator_initializer>>: init_]

variableDeclaratorId :: TTerm Java.Identifier -> TTerm (Maybe Java.Dims) -> TTerm Java.VariableDeclaratorId
variableDeclaratorId ident dims_ = record Java._VariableDeclaratorId [
  Java._VariableDeclaratorId_identifier>>: ident,
  Java._VariableDeclaratorId_dims>>: dims_]

variableDeclaratorIdIdentifier :: TTerm Java.VariableDeclaratorId -> TTerm Java.Identifier
variableDeclaratorIdIdentifier vid = project Java._VariableDeclaratorId Java._VariableDeclaratorId_identifier @@ vid

-- =============================================================================
-- VariableInitializer (union type)
-- =============================================================================

variableInitializerExpression :: TTerm Java.Expression -> TTerm Java.VariableInitializer
variableInitializerExpression = inject Java._VariableInitializer Java._VariableInitializer_expression

variableInitializerArray :: TTerm Java.ArrayInitializer -> TTerm Java.VariableInitializer
variableInitializerArray = inject Java._VariableInitializer Java._VariableInitializer_arrayInitializer

-- =============================================================================
-- Result (union type)
-- =============================================================================

resultType :: TTerm Java.UnannType -> TTerm Java.Result
resultType = inject Java._Result Java._Result_type

resultVoid :: TTerm Java.Result
resultVoid = injectUnit Java._Result Java._Result_void

-- =============================================================================
-- MethodBody (union type)
-- =============================================================================

methodBodyBlock :: TTerm Java.Block -> TTerm Java.MethodBody
methodBodyBlock = inject Java._MethodBody Java._MethodBody_block

methodBodyNone :: TTerm Java.MethodBody
methodBodyNone = injectUnit Java._MethodBody Java._MethodBody_none

-- =============================================================================
-- MethodDeclaration (record type)
-- =============================================================================

methodDeclaration_ :: TTerm [Java.Annotation] -> TTerm [Java.MethodModifier] -> TTerm Java.MethodHeader -> TTerm Java.MethodBody -> TTerm Java.MethodDeclaration
methodDeclaration_ anns mods header body = record Java._MethodDeclaration [
  Java._MethodDeclaration_annotations>>: anns,
  Java._MethodDeclaration_modifiers>>: mods,
  Java._MethodDeclaration_header>>: header,
  Java._MethodDeclaration_body>>: body]

-- =============================================================================
-- MethodHeader (record type)
-- =============================================================================

methodHeader :: TTerm [Java.TypeParameter] -> TTerm Java.Result -> TTerm Java.MethodDeclarator -> TTerm (Maybe Java.Throws) -> TTerm Java.MethodHeader
methodHeader tparams result decl throws = record Java._MethodHeader [
  Java._MethodHeader_parameters>>: tparams,
  Java._MethodHeader_result>>: result,
  Java._MethodHeader_declarator>>: decl,
  Java._MethodHeader_throws>>: throws]

-- =============================================================================
-- MethodDeclarator (record type)
-- =============================================================================

methodDeclarator :: TTerm Java.Identifier -> TTerm (Maybe Java.ReceiverParameter) -> TTerm [Java.FormalParameter] -> TTerm Java.MethodDeclarator
methodDeclarator ident recv params = record Java._MethodDeclarator [
  Java._MethodDeclarator_identifier>>: ident,
  Java._MethodDeclarator_receiverParameter>>: recv,
  Java._MethodDeclarator_formalParameters>>: params]

-- =============================================================================
-- FormalParameter (union type)
-- =============================================================================

formalParameterSimple :: TTerm Java.FormalParameter_Simple -> TTerm Java.FormalParameter
formalParameterSimple = inject Java._FormalParameter Java._FormalParameter_simple

formalParameterSimple_ :: TTerm [Java.VariableModifier] -> TTerm Java.UnannType -> TTerm Java.VariableDeclaratorId -> TTerm Java.FormalParameter_Simple
formalParameterSimple_ mods ut vid = record Java._FormalParameter_Simple [
  Java._FormalParameter_Simple_modifiers>>: mods,
  Java._FormalParameter_Simple_type>>: ut,
  Java._FormalParameter_Simple_id>>: vid]

-- =============================================================================
-- FieldDeclaration (record type)
-- =============================================================================

fieldDeclaration :: TTerm [Java.FieldModifier] -> TTerm Java.UnannType -> TTerm [Java.VariableDeclarator] -> TTerm Java.FieldDeclaration
fieldDeclaration mods ut vars = record Java._FieldDeclaration [
  Java._FieldDeclaration_modifiers>>: mods,
  Java._FieldDeclaration_unannType>>: ut,
  Java._FieldDeclaration_variableDeclarators>>: vars]

-- =============================================================================
-- ClassDeclaration (union type)
-- =============================================================================

classDeclarationNormal :: TTerm Java.NormalClassDeclaration -> TTerm Java.ClassDeclaration
classDeclarationNormal = inject Java._ClassDeclaration Java._ClassDeclaration_normal

-- =============================================================================
-- NormalClassDeclaration (record type)
-- =============================================================================

normalClassDeclaration :: TTerm [Java.ClassModifier] -> TTerm Java.TypeIdentifier -> TTerm [Java.TypeParameter] -> TTerm (Maybe Java.ClassType) -> TTerm [Java.InterfaceType] -> TTerm Java.ClassBody -> TTerm Java.NormalClassDeclaration
normalClassDeclaration mods ident tparams ext impl body = record Java._NormalClassDeclaration [
  Java._NormalClassDeclaration_modifiers>>: mods,
  Java._NormalClassDeclaration_identifier>>: ident,
  Java._NormalClassDeclaration_parameters>>: tparams,
  Java._NormalClassDeclaration_extends>>: ext,
  Java._NormalClassDeclaration_implements>>: impl,
  Java._NormalClassDeclaration_body>>: body]

normalClassDeclarationModifiers :: TTerm Java.NormalClassDeclaration -> TTerm [Java.ClassModifier]
normalClassDeclarationModifiers ncd = project Java._NormalClassDeclaration Java._NormalClassDeclaration_modifiers @@ ncd

normalClassDeclarationExtends :: TTerm Java.NormalClassDeclaration -> TTerm (Maybe Java.ClassType)
normalClassDeclarationExtends ncd = project Java._NormalClassDeclaration Java._NormalClassDeclaration_extends @@ ncd

normalClassDeclarationParameters :: TTerm Java.NormalClassDeclaration -> TTerm [Java.TypeParameter]
normalClassDeclarationParameters ncd = project Java._NormalClassDeclaration Java._NormalClassDeclaration_parameters @@ ncd

normalClassDeclarationBody :: TTerm Java.NormalClassDeclaration -> TTerm Java.ClassBody
normalClassDeclarationBody ncd = project Java._NormalClassDeclaration Java._NormalClassDeclaration_body @@ ncd

-- =============================================================================
-- ClassBody (wrapper type)
-- =============================================================================

classBody :: TTerm [Java.ClassBodyDeclarationWithComments] -> TTerm Java.ClassBody
classBody = wrap Java._ClassBody

unClassBody :: TTerm Java.ClassBody -> TTerm [Java.ClassBodyDeclarationWithComments]
unClassBody cb = unwrap Java._ClassBody @@ cb

-- =============================================================================
-- ClassBodyDeclaration (union type)
-- =============================================================================

classBodyDeclClassMember :: TTerm Java.ClassMemberDeclaration -> TTerm Java.ClassBodyDeclaration
classBodyDeclClassMember = inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_classMember

classBodyDeclConstructor :: TTerm Java.ConstructorDeclaration -> TTerm Java.ClassBodyDeclaration
classBodyDeclConstructor = inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_constructorDeclaration

-- =============================================================================
-- ClassBodyDeclarationWithComments (record type)
-- =============================================================================

classBodyDeclarationWithComments :: TTerm Java.ClassBodyDeclaration -> TTerm (Maybe String) -> TTerm Java.ClassBodyDeclarationWithComments
classBodyDeclarationWithComments decl comments = record Java._ClassBodyDeclarationWithComments [
  Java._ClassBodyDeclarationWithComments_value>>: decl,
  Java._ClassBodyDeclarationWithComments_comments>>: comments]

-- =============================================================================
-- ClassMemberDeclaration (union type)
-- =============================================================================

classMemberDeclField :: TTerm Java.FieldDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclField = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_field

classMemberDeclMethod :: TTerm Java.MethodDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclMethod = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_method

classMemberDeclClass :: TTerm Java.ClassDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclClass = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_class

classMemberDeclInterface :: TTerm Java.InterfaceDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclInterface = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_interface

-- =============================================================================
-- InterfaceDeclaration (union type)
-- =============================================================================

interfaceDeclarationNormalInterface :: TTerm Java.NormalInterfaceDeclaration -> TTerm Java.InterfaceDeclaration
interfaceDeclarationNormalInterface = inject Java._InterfaceDeclaration Java._InterfaceDeclaration_normalInterface

-- =============================================================================
-- NormalInterfaceDeclaration (record type)
-- =============================================================================

normalInterfaceDeclaration :: TTerm [Java.InterfaceModifier] -> TTerm Java.TypeIdentifier -> TTerm [Java.TypeParameter] -> TTerm [Java.InterfaceType] -> TTerm Java.InterfaceBody -> TTerm Java.NormalInterfaceDeclaration
normalInterfaceDeclaration mods ident tparams extends body = record Java._NormalInterfaceDeclaration [
  Java._NormalInterfaceDeclaration_modifiers>>: mods,
  Java._NormalInterfaceDeclaration_identifier>>: ident,
  Java._NormalInterfaceDeclaration_parameters>>: tparams,
  Java._NormalInterfaceDeclaration_extends>>: extends,
  Java._NormalInterfaceDeclaration_body>>: body]

-- =============================================================================
-- InterfaceBody (wrapper type)
-- =============================================================================

interfaceBody :: TTerm [Java.InterfaceMemberDeclaration] -> TTerm Java.InterfaceBody
interfaceBody = wrap Java._InterfaceBody

-- =============================================================================
-- InterfaceMemberDeclaration (union type)
-- =============================================================================

interfaceMemberDeclConstant :: TTerm Java.ConstantDeclaration -> TTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclConstant = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_constant

interfaceMemberDeclInterfaceMethod :: TTerm Java.InterfaceMethodDeclaration -> TTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclInterfaceMethod = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_interfaceMethod

interfaceMemberDeclClass :: TTerm Java.ClassDeclaration -> TTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclClass = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_class

-- =============================================================================
-- InterfaceMethodDeclaration (record type)
-- =============================================================================

interfaceMethodDeclaration_ :: TTerm [Java.InterfaceMethodModifier] -> TTerm Java.MethodHeader -> TTerm Java.MethodBody -> TTerm Java.InterfaceMethodDeclaration
interfaceMethodDeclaration_ mods header body = record Java._InterfaceMethodDeclaration [
  Java._InterfaceMethodDeclaration_modifiers>>: mods,
  Java._InterfaceMethodDeclaration_header>>: header,
  Java._InterfaceMethodDeclaration_body>>: body]

-- =============================================================================
-- ConstantDeclaration (record type)
-- =============================================================================

constantDeclaration :: TTerm [Java.ConstantModifier] -> TTerm Java.UnannType -> TTerm [Java.VariableDeclarator] -> TTerm Java.ConstantDeclaration
constantDeclaration mods ut vars = record Java._ConstantDeclaration [
  Java._ConstantDeclaration_modifiers>>: mods,
  Java._ConstantDeclaration_type>>: ut,
  Java._ConstantDeclaration_variables>>: vars]

-- =============================================================================
-- ConstructorDeclaration (record type)
-- =============================================================================

constructorDeclaration :: TTerm [Java.ConstructorModifier] -> TTerm Java.ConstructorDeclarator -> TTerm (Maybe Java.Throws) -> TTerm Java.ConstructorBody -> TTerm Java.ConstructorDeclaration
constructorDeclaration mods decl throws body = record Java._ConstructorDeclaration [
  Java._ConstructorDeclaration_modifiers>>: mods,
  Java._ConstructorDeclaration_constructor>>: decl,
  Java._ConstructorDeclaration_throws>>: throws,
  Java._ConstructorDeclaration_body>>: body]

constructorDeclarator :: TTerm [Java.TypeParameter] -> TTerm Java.SimpleTypeName -> TTerm (Maybe Java.ReceiverParameter) -> TTerm [Java.FormalParameter] -> TTerm Java.ConstructorDeclarator
constructorDeclarator tparams name recv params = record Java._ConstructorDeclarator [
  Java._ConstructorDeclarator_parameters>>: tparams,
  Java._ConstructorDeclarator_name>>: name,
  Java._ConstructorDeclarator_receiverParameter>>: recv,
  Java._ConstructorDeclarator_formalParameters>>: params]

constructorBody :: TTerm (Maybe Java.ExplicitConstructorInvocation) -> TTerm [Java.BlockStatement] -> TTerm Java.ConstructorBody
constructorBody invocation stmts = record Java._ConstructorBody [
  Java._ConstructorBody_invocation>>: invocation,
  Java._ConstructorBody_statements>>: stmts]

-- =============================================================================
-- SimpleTypeName (wrapper type)
-- =============================================================================

simpleTypeName :: TTerm Java.TypeIdentifier -> TTerm Java.SimpleTypeName
simpleTypeName = wrap Java._SimpleTypeName

-- =============================================================================
-- Annotation (union type)
-- =============================================================================

annotationNormal :: TTerm Java.NormalAnnotation -> TTerm Java.Annotation
annotationNormal = inject Java._Annotation Java._Annotation_normal

annotationMarker :: TTerm Java.MarkerAnnotation -> TTerm Java.Annotation
annotationMarker = inject Java._Annotation Java._Annotation_marker

annotationSingleElement :: TTerm Java.SingleElementAnnotation -> TTerm Java.Annotation
annotationSingleElement = inject Java._Annotation Java._Annotation_singleElement

-- =============================================================================
-- MarkerAnnotation (wrapper type)
-- =============================================================================

markerAnnotation :: TTerm Java.TypeName -> TTerm Java.MarkerAnnotation
markerAnnotation = wrap Java._MarkerAnnotation

-- =============================================================================
-- NormalAnnotation (record type)
-- =============================================================================

normalAnnotation :: TTerm Java.TypeName -> TTerm [Java.ElementValuePair] -> TTerm Java.NormalAnnotation
normalAnnotation tn pairs = record Java._NormalAnnotation [
  Java._NormalAnnotation_typeName>>: tn,
  Java._NormalAnnotation_pairs>>: pairs]

elementValuePair :: TTerm Java.Identifier -> TTerm Java.ElementValue -> TTerm Java.ElementValuePair
elementValuePair key val = record Java._ElementValuePair [
  Java._ElementValuePair_key>>: key,
  Java._ElementValuePair_value>>: val]

-- =============================================================================
-- ElementValue (union type)
-- =============================================================================

elementValueConditional :: TTerm Java.ConditionalExpression -> TTerm Java.ElementValue
elementValueConditional = inject Java._ElementValue Java._ElementValue_conditionalExpression

elementValueArray :: TTerm Java.ElementValueArrayInitializer -> TTerm Java.ElementValue
elementValueArray = inject Java._ElementValue Java._ElementValue_elementValueArrayInitializer

-- =============================================================================
-- CompilationUnit (union type)
-- =============================================================================

compilationUnitOrdinary :: TTerm Java.OrdinaryCompilationUnit -> TTerm Java.CompilationUnit
compilationUnitOrdinary = inject Java._CompilationUnit Java._CompilationUnit_ordinary

-- =============================================================================
-- OrdinaryCompilationUnit (record type)
-- =============================================================================

ordinaryCompilationUnit :: TTerm (Maybe Java.PackageDeclaration) -> TTerm [Java.ImportDeclaration] -> TTerm [Java.TypeDeclarationWithComments] -> TTerm Java.OrdinaryCompilationUnit
ordinaryCompilationUnit pkg imports types = record Java._OrdinaryCompilationUnit [
  Java._OrdinaryCompilationUnit_package>>: pkg,
  Java._OrdinaryCompilationUnit_imports>>: imports,
  Java._OrdinaryCompilationUnit_types>>: types]

-- =============================================================================
-- PackageDeclaration (record type)
-- =============================================================================

packageDeclaration :: TTerm [Java.PackageModifier] -> TTerm [Java.Identifier] -> TTerm Java.PackageDeclaration
packageDeclaration mods idents = record Java._PackageDeclaration [
  Java._PackageDeclaration_modifiers>>: mods,
  Java._PackageDeclaration_identifiers>>: idents]

-- =============================================================================
-- ImportDeclaration (union type)
-- =============================================================================

importDeclarationSingleType :: TTerm Java.SingleTypeImportDeclaration -> TTerm Java.ImportDeclaration
importDeclarationSingleType = inject Java._ImportDeclaration Java._ImportDeclaration_singleType

singleTypeImportDeclaration :: TTerm Java.TypeName -> TTerm Java.SingleTypeImportDeclaration
singleTypeImportDeclaration = wrap Java._SingleTypeImportDeclaration

-- =============================================================================
-- TypeDeclaration (union type)
-- =============================================================================

typeDeclarationClass :: TTerm Java.ClassDeclaration -> TTerm Java.TypeDeclaration
typeDeclarationClass = inject Java._TypeDeclaration Java._TypeDeclaration_class

typeDeclarationInterface :: TTerm Java.InterfaceDeclaration -> TTerm Java.TypeDeclaration
typeDeclarationInterface = inject Java._TypeDeclaration Java._TypeDeclaration_interface

-- =============================================================================
-- TypeDeclarationWithComments (record type)
-- =============================================================================

typeDeclarationWithComments :: TTerm Java.TypeDeclaration -> TTerm (Maybe String) -> TTerm Java.TypeDeclarationWithComments
typeDeclarationWithComments decl comments = record Java._TypeDeclarationWithComments [
  Java._TypeDeclarationWithComments_value>>: decl,
  Java._TypeDeclarationWithComments_comments>>: comments]

-- =============================================================================
-- ArrayCreationExpression (union type)
-- =============================================================================

arrayCreationExpressionPrimitiveArray :: TTerm Java.ArrayCreationExpression_PrimitiveArray -> TTerm Java.ArrayCreationExpression
arrayCreationExpressionPrimitiveArray = inject Java._ArrayCreationExpression Java._ArrayCreationExpression_primitiveArray

arrayCreationExpressionPrimitiveArray_ :: TTerm Java.PrimitiveTypeWithAnnotations -> TTerm Java.Dims -> TTerm Java.ArrayInitializer -> TTerm Java.ArrayCreationExpression_PrimitiveArray
arrayCreationExpressionPrimitiveArray_ pt dims_ arr = record Java._ArrayCreationExpression_PrimitiveArray [
  Java._ArrayCreationExpression_PrimitiveArray_type>>: pt,
  Java._ArrayCreationExpression_PrimitiveArray_dims>>: dims_,
  Java._ArrayCreationExpression_PrimitiveArray_array>>: arr]

-- =============================================================================
-- ArrayInitializer (wrapper type)
-- =============================================================================

arrayInitializer :: TTerm [Java.VariableInitializer] -> TTerm Java.ArrayInitializer
arrayInitializer = wrap Java._ArrayInitializer

-- =============================================================================
-- Composition helpers (multi-level conversions)
-- =============================================================================

-- | Literal -> Primary
literalToPrimary :: TTerm Java.Literal -> TTerm Java.Primary
literalToPrimary lit = primaryNoNewArray (primaryLiteral lit)

-- | Literal -> Expression
literalToExpression :: TTerm Java.Literal -> TTerm Java.Expression
literalToExpression lit = primaryToExpression (literalToPrimary lit)

-- | Primary -> Expression
primaryToExpression :: TTerm Java.Primary -> TTerm Java.Expression
primaryToExpression p = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          inclusiveOrExpression (list [
            exclusiveOrExpression (list [
              andExpression (list [
                equalityExpressionUnary
                  (relationalExpressionSimple
                    (shiftExpressionUnary
                      (additiveExpressionUnary
                        (multiplicativeExpressionUnary
                          (unaryExpressionOther
                            (unaryExpressionNotPlusMinusPostfix
                              (postfixExpressionPrimary p)))))))])])])])]))))

-- | MethodInvocation -> Expression
methodInvocationToExpression :: TTerm Java.MethodInvocation -> TTerm Java.Expression
methodInvocationToExpression mi = primaryToExpression (primaryNoNewArray (primaryMethodInvocation mi))

-- | MethodInvocation -> Primary
methodInvocationToPrimary :: TTerm Java.MethodInvocation -> TTerm Java.Primary
methodInvocationToPrimary mi = primaryNoNewArray (primaryMethodInvocation mi)

-- | MethodInvocation -> Statement
methodInvocationToStatement :: TTerm Java.MethodInvocation -> TTerm Java.Statement
methodInvocationToStatement mi = statementWithoutTrailing
  (stmtExpression (expressionStatement (stmtExprMethodInvocation mi)))

-- | PostfixExpression -> Expression
postfixToExpression :: TTerm Java.PostfixExpression -> TTerm Java.Expression
postfixToExpression pe = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          inclusiveOrExpression (list [
            exclusiveOrExpression (list [
              andExpression (list [
                equalityExpressionUnary
                  (relationalExpressionSimple
                    (shiftExpressionUnary
                      (additiveExpressionUnary
                        (multiplicativeExpressionUnary
                          (unaryExpressionOther
                            (unaryExpressionNotPlusMinusPostfix pe))))))])])])])]))))

-- | ExpressionName -> Expression
expressionNameToExpression :: TTerm Java.ExpressionName -> TTerm Java.Expression
expressionNameToExpression en = postfixToExpression (postfixExpressionName en)

-- | Identifier -> Expression (as expression name)
identifierToExpression :: TTerm Java.Identifier -> TTerm Java.Expression
identifierToExpression id_ = expressionNameToExpression (expressionName nothing id_)

-- | FieldAccess -> Expression
fieldAccessToExpression :: TTerm Java.FieldAccess -> TTerm Java.Expression
fieldAccessToExpression fa = primaryToExpression (primaryNoNewArray (primaryFieldAccess fa))

-- | CastExpression -> Expression
castExpressionToExpression :: TTerm Java.CastExpression -> TTerm Java.Expression
castExpressionToExpression ce = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          inclusiveOrExpression (list [
            exclusiveOrExpression (list [
              andExpression (list [
                equalityExpressionUnary
                  (relationalExpressionSimple
                    (shiftExpressionUnary
                      (additiveExpressionUnary
                        (multiplicativeExpressionUnary
                          (unaryExpressionOther
                            (unaryExpressionNotPlusMinusCast ce))))))])])])])]))))

-- | Expression -> Primary (via parens)
expressionToPrimary :: TTerm Java.Expression -> TTerm Java.Primary
expressionToPrimary e = primaryNoNewArray (primaryParens e)

-- | Expression -> UnaryExpression
expressionToUnary :: TTerm Java.Expression -> TTerm Java.UnaryExpression
expressionToUnary e = unaryExpressionOther
  (unaryExpressionNotPlusMinusPostfix
    (postfixExpressionPrimary (expressionToPrimary e)))

-- | Identifier -> UnaryExpression (via expression name)
identifierToUnary :: TTerm Java.Identifier -> TTerm Java.UnaryExpression
identifierToUnary id_ = unaryExpressionOther
  (unaryExpressionNotPlusMinusPostfix
    (postfixExpressionName (expressionName nothing id_)))

-- | Primary -> UnaryExpression
primaryToUnary :: TTerm Java.UnaryExpression -> TTerm Java.Primary
primaryToUnary ue = primaryNoNewArray (primaryParens (unaryToExpression ue))

-- | UnaryExpression -> Expression
unaryToExpression :: TTerm Java.UnaryExpression -> TTerm Java.Expression
unaryToExpression ue = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          inclusiveOrExpression (list [
            exclusiveOrExpression (list [
              andExpression (list [
                equalityExpressionUnary
                  (relationalExpressionSimple
                    (shiftExpressionUnary
                      (additiveExpressionUnary
                        (multiplicativeExpressionUnary ue))))])])])])]))))

-- | PostfixExpression -> InclusiveOrExpression
postfixToInclusiveOr :: TTerm Java.PostfixExpression -> TTerm Java.InclusiveOrExpression
postfixToInclusiveOr pe = inclusiveOrExpression (list [
  exclusiveOrExpression (list [
    andExpression (list [
      equalityExpressionUnary
        (relationalExpressionSimple
          (shiftExpressionUnary
            (additiveExpressionUnary
              (multiplicativeExpressionUnary
                (unaryExpressionOther
                  (unaryExpressionNotPlusMinusPostfix pe))))))])])])

-- | EqualityExpression -> InclusiveOrExpression
equalityToInclusiveOr :: TTerm Java.EqualityExpression -> TTerm Java.InclusiveOrExpression
equalityToInclusiveOr ee = inclusiveOrExpression (list [
  exclusiveOrExpression (list [
    andExpression (list [ee])])])

-- | EqualityExpression -> Expression
equalityToExpression :: TTerm Java.EqualityExpression -> TTerm Java.Expression
equalityToExpression ee = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          equalityToInclusiveOr ee])]))))

-- | ConditionalAndExpression -> Expression
conditionalAndToExpression :: TTerm Java.ConditionalAndExpression -> TTerm Java.Expression
conditionalAndToExpression cae = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [cae]))))

-- | AdditiveExpression -> Expression
additiveToExpression :: TTerm Java.AdditiveExpression -> TTerm Java.Expression
additiveToExpression ae = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          inclusiveOrExpression (list [
            exclusiveOrExpression (list [
              andExpression (list [
                equalityExpressionUnary
                  (relationalExpressionSimple
                    (shiftExpressionUnary ae))])])])])]))))

-- | Literal -> RelationalExpression
literalToRelationalExpression :: TTerm Java.Literal -> TTerm Java.RelationalExpression
literalToRelationalExpression lit =
  relationalExpressionSimple
    (shiftExpressionUnary
      (additiveExpressionUnary
        (multiplicativeExpressionUnary
          (unaryExpressionOther
            (unaryExpressionNotPlusMinusPostfix
              (postfixExpressionPrimary (literalToPrimary lit)))))))

-- | PostfixExpression -> RelationalExpression
postfixToRelationalExpression :: TTerm Java.PostfixExpression -> TTerm Java.RelationalExpression
postfixToRelationalExpression pe =
  relationalExpressionSimple
    (shiftExpressionUnary
      (additiveExpressionUnary
        (multiplicativeExpressionUnary
          (unaryExpressionOther
            (unaryExpressionNotPlusMinusPostfix pe)))))

-- | MethodInvocation -> PostfixExpression
methodInvocationToPostfix :: TTerm Java.MethodInvocation -> TTerm Java.PostfixExpression
methodInvocationToPostfix mi = postfixExpressionPrimary (primaryNoNewArray (primaryMethodInvocation mi))

-- | Literal -> MultiplicativeExpression (for use in string concatenation)
literalToMultiplicativeExpression :: TTerm Java.Literal -> TTerm Java.MultiplicativeExpression
literalToMultiplicativeExpression lit =
  multiplicativeExpressionUnary
    (unaryExpressionOther
      (unaryExpressionNotPlusMinusPostfix
        (postfixExpressionPrimary (literalToPrimary lit))))

-- | RelationalExpression -> UnaryExpression (for instanceof negation: !(x instanceof T))
relationalToUnary :: TTerm Java.RelationalExpression -> TTerm Java.UnaryExpression
relationalToUnary re = unaryExpressionOther
  (unaryExpressionNotPlusMinusPostfix
    (postfixExpressionPrimary
      (primaryNoNewArray
        (primaryParens
          (expressionAssignment
            (assignmentExpressionConditional
              (conditionalExpressionSimple
                (conditionalOrExpression (list [
                  conditionalAndExpression (list [
                    inclusiveOrExpression (list [
                      exclusiveOrExpression (list [
                        andExpression (list [
                          equalityExpressionUnary re])])])])])))))))))

-- | Identifier -> RelationalExpression
identifierToRelationalExpression :: TTerm Java.Identifier -> TTerm Java.RelationalExpression
identifierToRelationalExpression id_ =
  postfixToRelationalExpression (postfixExpressionName (expressionName nothing id_))
