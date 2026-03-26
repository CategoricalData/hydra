-- | Convenience layer over the generated Java syntax DSL.
-- Re-exports all generated DSL functions and adds custom helpers
-- for common Java AST construction patterns.

module Hydra.Ext.Dsl.Java.Syntax (
  module Hydra.Dsl.Ext.Java.Syntax,
  module Hydra.Ext.Dsl.Java.Syntax,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Ext.Java.Syntax hiding (
  -- Hide functions whose types have changed since the hand-written DSL was created.
  -- The hand-written versions are kept below for backward compatibility.
  arrayInitializer,
  classInstanceCreationExpression,
  lambdaParametersTuple,
  variableDeclaratorId)
import qualified Hydra.Ext.Java.Syntax as Java

import Prelude hiding (map)


-- Functions with signatures that differ from the generated versions.
-- These use the original types for backward compatibility with existing call sites.

arrayInitializer :: TTerm [Java.VariableInitializer] -> TTerm Java.ArrayInitializer
arrayInitializer = wrap Java._ArrayInitializer

classInstanceCreationExpression :: TTerm (Maybe Java.Expression) -> TTerm Java.UnqualifiedClassInstanceCreationExpression -> TTerm Java.ClassInstanceCreationExpression
classInstanceCreationExpression qual unqual = record Java._ClassInstanceCreationExpression [
  Java._ClassInstanceCreationExpression_qualifier>>: qual,
  Java._ClassInstanceCreationExpression_expression>>: unqual]

lambdaParametersTuple :: TTerm [Java.FormalParameter] -> TTerm Java.LambdaParameters
lambdaParametersTuple = inject Java._LambdaParameters Java._LambdaParameters_tuple

variableDeclaratorId :: TTerm Java.Identifier -> TTerm (Maybe Java.Dims) -> TTerm Java.VariableDeclaratorId
variableDeclaratorId ident dims_ = record Java._VariableDeclaratorId [
  Java._VariableDeclaratorId_identifier>>: ident,
  Java._VariableDeclaratorId_dims>>: dims_]


-- Custom helpers: expression type conversions

literalToPrimary :: TTerm Java.Literal -> TTerm Java.Primary
literalToPrimary lit = primaryNoNewArray (primaryLiteral lit)

literalToExpression :: TTerm Java.Literal -> TTerm Java.Expression
literalToExpression lit = primaryToExpression (literalToPrimary lit)

literalToRelationalExpression :: TTerm Java.Literal -> TTerm Java.RelationalExpression
literalToRelationalExpression lit =
  relationalExpressionSimple
    (shiftExpressionUnary
      (additiveExpressionUnary
        (multiplicativeExpressionUnary
          (unaryExpressionOther
            (unaryExpressionNotPlusMinusPostfix
              (postfixExpressionPrimary (literalToPrimary lit)))))))

literalToMultiplicativeExpression :: TTerm Java.Literal -> TTerm Java.MultiplicativeExpression
literalToMultiplicativeExpression lit =
  multiplicativeExpressionUnary
    (unaryExpressionOther
      (unaryExpressionNotPlusMinusPostfix
        (postfixExpressionPrimary (literalToPrimary lit))))

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

postfixToRelationalExpression :: TTerm Java.PostfixExpression -> TTerm Java.RelationalExpression
postfixToRelationalExpression pe =
  relationalExpressionSimple
    (shiftExpressionUnary
      (additiveExpressionUnary
        (multiplicativeExpressionUnary
          (unaryExpressionOther
            (unaryExpressionNotPlusMinusPostfix pe)))))

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

equalityToExpression :: TTerm Java.EqualityExpression -> TTerm Java.Expression
equalityToExpression ee = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          equalityToInclusiveOr ee])]))))

equalityToInclusiveOr :: TTerm Java.EqualityExpression -> TTerm Java.InclusiveOrExpression
equalityToInclusiveOr ee = inclusiveOrExpression (list [
  exclusiveOrExpression (list [
    andExpression (list [ee])])])

conditionalAndToExpression :: TTerm Java.ConditionalAndExpression -> TTerm Java.Expression
conditionalAndToExpression cae = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [cae]))))

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

expressionNameToExpression :: TTerm Java.ExpressionName -> TTerm Java.Expression
expressionNameToExpression en = postfixToExpression (postfixExpressionName en)

identifierToExpression :: TTerm Java.Identifier -> TTerm Java.Expression
identifierToExpression id_ = expressionNameToExpression (expressionName nothing id_)

identifierToRelationalExpression :: TTerm Java.Identifier -> TTerm Java.RelationalExpression
identifierToRelationalExpression id_ =
  postfixToRelationalExpression (postfixExpressionName (expressionName nothing id_))

identifierToUnary :: TTerm Java.Identifier -> TTerm Java.UnaryExpression
identifierToUnary id_ = unaryExpressionOther
  (unaryExpressionNotPlusMinusPostfix
    (postfixExpressionName (expressionName nothing id_)))

fieldAccessToExpression :: TTerm Java.FieldAccess -> TTerm Java.Expression
fieldAccessToExpression fa = primaryToExpression (primaryNoNewArray (primaryFieldAccess fa))

methodInvocationToExpression :: TTerm Java.MethodInvocation -> TTerm Java.Expression
methodInvocationToExpression mi = primaryToExpression (primaryNoNewArray (primaryMethodInvocation mi))

methodInvocationToPrimary :: TTerm Java.MethodInvocation -> TTerm Java.Primary
methodInvocationToPrimary mi = primaryNoNewArray (primaryMethodInvocation mi)

methodInvocationToPostfix :: TTerm Java.MethodInvocation -> TTerm Java.PostfixExpression
methodInvocationToPostfix mi = postfixExpressionPrimary (primaryNoNewArray (primaryMethodInvocation mi))

methodInvocationToStatement :: TTerm Java.MethodInvocation -> TTerm Java.Statement
methodInvocationToStatement mi = statementWithoutTrailing
  (stmtExpression (expressionStatement (stmtExprMethodInvocation mi)))

expressionToPrimary :: TTerm Java.Expression -> TTerm Java.Primary
expressionToPrimary e = primaryNoNewArray (primaryParens e)

expressionToUnary :: TTerm Java.Expression -> TTerm Java.UnaryExpression
expressionToUnary e = unaryExpressionOther
  (unaryExpressionNotPlusMinusPostfix
    (postfixExpressionPrimary (expressionToPrimary e)))

primaryToUnary :: TTerm Java.UnaryExpression -> TTerm Java.Primary
primaryToUnary ue = primaryNoNewArray (primaryParens (unaryToExpression ue))

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


-- Custom helpers: binary expression constructors

additiveExpressionBinary :: TTerm Java.AdditiveExpression -> TTerm Java.MultiplicativeExpression -> TTerm Java.AdditiveExpression_Binary
additiveExpressionBinary lhs rhs = record Java._AdditiveExpression_Binary [
  Java._AdditiveExpression_Binary_lhs>>: lhs,
  Java._AdditiveExpression_Binary_rhs>>: rhs]

equalityExpressionBinary :: TTerm Java.EqualityExpression -> TTerm Java.RelationalExpression -> TTerm Java.EqualityExpression_Binary
equalityExpressionBinary lhs rhs = record Java._EqualityExpression_Binary [
  Java._EqualityExpression_Binary_lhs>>: lhs,
  Java._EqualityExpression_Binary_rhs>>: rhs]

multiplicativeExpressionBinary :: TTerm Java.MultiplicativeExpression -> TTerm Java.UnaryExpression -> TTerm Java.MultiplicativeExpression_Binary
multiplicativeExpressionBinary lhs rhs = record Java._MultiplicativeExpression_Binary [
  Java._MultiplicativeExpression_Binary_lhs>>: lhs,
  Java._MultiplicativeExpression_Binary_rhs>>: rhs]


-- Custom helpers: primary constructors

primaryLiteral :: TTerm Java.Literal -> TTerm Java.PrimaryNoNewArrayExpression
primaryLiteral = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_literal

primaryParens :: TTerm Java.Expression -> TTerm Java.PrimaryNoNewArrayExpression
primaryParens = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_parens

primaryFieldAccess :: TTerm Java.FieldAccess -> TTerm Java.PrimaryNoNewArrayExpression
primaryFieldAccess = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_fieldAccess

primaryMethodInvocation :: TTerm Java.MethodInvocation -> TTerm Java.PrimaryNoNewArrayExpression
primaryMethodInvocation = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_methodInvocation

primaryThis :: TTerm Java.PrimaryNoNewArrayExpression
primaryThis = injectUnit Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_this

primaryClassInstance :: TTerm Java.ClassInstanceCreationExpression -> TTerm Java.PrimaryNoNewArrayExpression
primaryClassInstance = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_classInstance

primaryArrayAccess :: TTerm Java.ArrayAccess -> TTerm Java.PrimaryNoNewArrayExpression
primaryArrayAccess = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_arrayAccess


-- Custom helpers: statement constructors

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

stmtExprAssignment :: TTerm Java.Assignment -> TTerm Java.StatementExpression
stmtExprAssignment = inject Java._StatementExpression Java._StatementExpression_assignment

stmtExprMethodInvocation :: TTerm Java.MethodInvocation -> TTerm Java.StatementExpression
stmtExprMethodInvocation = inject Java._StatementExpression Java._StatementExpression_methodInvocation

stmtExprClassInstance :: TTerm Java.ClassInstanceCreationExpression -> TTerm Java.StatementExpression
stmtExprClassInstance = inject Java._StatementExpression Java._StatementExpression_classInstanceCreation


-- Custom helpers: other constructors

arrayCreationExpressionPrimitiveArray_ :: TTerm Java.PrimitiveTypeWithAnnotations -> TTerm Java.Dims -> TTerm Java.ArrayInitializer -> TTerm Java.ArrayCreationExpression_PrimitiveArray
arrayCreationExpressionPrimitiveArray_ pt dims_ arr = record Java._ArrayCreationExpression_PrimitiveArray [
  Java._ArrayCreationExpression_PrimitiveArray_type>>: pt,
  Java._ArrayCreationExpression_PrimitiveArray_dims>>: dims_,
  Java._ArrayCreationExpression_PrimitiveArray_array>>: arr]

arrayTypeVariantPrimitive :: TTerm Java.PrimitiveTypeWithAnnotations -> TTerm Java.ArrayType_Variant
arrayTypeVariantPrimitive = inject Java._ArrayType_Variant Java._ArrayType_Variant_primitive

castExpressionNotPlusMinus_ :: TTerm Java.CastExpression_RefAndBounds -> TTerm Java.UnaryExpressionNotPlusMinus -> TTerm Java.CastExpression_NotPlusMinus
castExpressionNotPlusMinus_ rb expr = record Java._CastExpression_NotPlusMinus [
  Java._CastExpression_NotPlusMinus_refAndBounds>>: rb,
  Java._CastExpression_NotPlusMinus_expression>>: expr]

castExpressionPrimitive_ :: TTerm Java.PrimitiveTypeWithAnnotations -> TTerm Java.UnaryExpression -> TTerm Java.CastExpression_Primitive
castExpressionPrimitive_ pt expr = record Java._CastExpression_Primitive [
  Java._CastExpression_Primitive_type>>: pt,
  Java._CastExpression_Primitive_expression>>: expr]

castExpressionRefAndBounds :: TTerm Java.ReferenceType -> TTerm [Java.AdditionalBound] -> TTerm Java.CastExpression_RefAndBounds
castExpressionRefAndBounds rt bounds = record Java._CastExpression_RefAndBounds [
  Java._CastExpression_RefAndBounds_type>>: rt,
  Java._CastExpression_RefAndBounds_bounds>>: bounds]

fieldAccessQualifierPrimary :: TTerm Java.Primary -> TTerm Java.FieldAccess_Qualifier
fieldAccessQualifierPrimary = inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary

relationalExpressionInstanceOf :: TTerm Java.RelationalExpression_InstanceOf -> TTerm Java.RelationalExpression
relationalExpressionInstanceOf = inject Java._RelationalExpression Java._RelationalExpression_instanceof

relationalExpressionInstanceOf_ :: TTerm Java.RelationalExpression -> TTerm Java.ReferenceType -> TTerm Java.RelationalExpression_InstanceOf
relationalExpressionInstanceOf_ expr rt = record Java._RelationalExpression_InstanceOf [
  Java._RelationalExpression_InstanceOf_lhs>>: expr,
  Java._RelationalExpression_InstanceOf_rhs>>: rt]

methodInvocation_ :: TTerm Java.MethodInvocation_Header -> TTerm [Java.Expression] -> TTerm Java.MethodInvocation
methodInvocation_ header args = record Java._MethodInvocation [
  Java._MethodInvocation_header>>: header,
  Java._MethodInvocation_arguments>>: args]

methodInvocationHeaderSimple :: TTerm Java.MethodName -> TTerm Java.MethodInvocation_Header
methodInvocationHeaderSimple = inject Java._MethodInvocation_Header Java._MethodInvocation_Header_simple

methodInvocationHeaderComplex :: TTerm Java.MethodInvocation_Complex -> TTerm Java.MethodInvocation_Header
methodInvocationHeaderComplex = inject Java._MethodInvocation_Header Java._MethodInvocation_Header_complex

methodInvocationComplex :: TTerm Java.MethodInvocation_Variant -> TTerm [Java.TypeArgument] -> TTerm Java.Identifier -> TTerm Java.MethodInvocation_Complex
methodInvocationComplex variant targs ident = record Java._MethodInvocation_Complex [
  Java._MethodInvocation_Complex_variant>>: variant,
  Java._MethodInvocation_Complex_typeArguments>>: targs,
  Java._MethodInvocation_Complex_identifier>>: ident]

methodInvocationVariantExpression :: TTerm Java.ExpressionName -> TTerm Java.MethodInvocation_Variant
methodInvocationVariantExpression = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_expression

methodInvocationVariantPrimary :: TTerm Java.Primary -> TTerm Java.MethodInvocation_Variant
methodInvocationVariantPrimary = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_primary

methodInvocationVariantType :: TTerm Java.TypeName -> TTerm Java.MethodInvocation_Variant
methodInvocationVariantType = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_type

methodDeclaration_ :: TTerm [Java.Annotation] -> TTerm [Java.MethodModifier] -> TTerm Java.MethodHeader -> TTerm Java.MethodBody -> TTerm Java.MethodDeclaration
methodDeclaration_ anns mods header body = record Java._MethodDeclaration [
  Java._MethodDeclaration_annotations>>: anns,
  Java._MethodDeclaration_modifiers>>: mods,
  Java._MethodDeclaration_header>>: header,
  Java._MethodDeclaration_body>>: body]

interfaceMethodDeclaration_ :: TTerm [Java.InterfaceMethodModifier] -> TTerm Java.MethodHeader -> TTerm Java.MethodBody -> TTerm Java.InterfaceMethodDeclaration
interfaceMethodDeclaration_ mods header body = record Java._InterfaceMethodDeclaration [
  Java._InterfaceMethodDeclaration_modifiers>>: mods,
  Java._InterfaceMethodDeclaration_header>>: header,
  Java._InterfaceMethodDeclaration_body>>: body]

formalParameterSimple_ :: TTerm [Java.VariableModifier] -> TTerm Java.UnannType -> TTerm Java.VariableDeclaratorId -> TTerm Java.FormalParameter_Simple
formalParameterSimple_ mods ut vid = record Java._FormalParameter_Simple [
  Java._FormalParameter_Simple_modifiers>>: mods,
  Java._FormalParameter_Simple_type>>: ut,
  Java._FormalParameter_Simple_id>>: vid]

classBodyDeclClassMember :: TTerm Java.ClassMemberDeclaration -> TTerm Java.ClassBodyDeclaration
classBodyDeclClassMember = inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_classMember

classBodyDeclConstructor :: TTerm Java.ConstructorDeclaration -> TTerm Java.ClassBodyDeclaration
classBodyDeclConstructor = inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_constructorDeclaration

classMemberDeclField :: TTerm Java.FieldDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclField = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_field

classMemberDeclMethod :: TTerm Java.MethodDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclMethod = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_method

classMemberDeclClass :: TTerm Java.ClassDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclClass = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_class

classMemberDeclInterface :: TTerm Java.InterfaceDeclaration -> TTerm Java.ClassMemberDeclaration
classMemberDeclInterface = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_interface

interfaceMemberDeclConstant :: TTerm Java.ConstantDeclaration -> TTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclConstant = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_constant

interfaceMemberDeclInterfaceMethod :: TTerm Java.InterfaceMethodDeclaration -> TTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclInterfaceMethod = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_interfaceMethod

interfaceMemberDeclClass :: TTerm Java.ClassDeclaration -> TTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclClass = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_class

elementValueConditional :: TTerm Java.ConditionalExpression -> TTerm Java.ElementValue
elementValueConditional = inject Java._ElementValue Java._ElementValue_conditionalExpression

elementValueArray :: TTerm Java.ElementValueArrayInitializer -> TTerm Java.ElementValue
elementValueArray = inject Java._ElementValue Java._ElementValue_elementValueArrayInitializer

variableInitializerArray :: TTerm Java.ArrayInitializer -> TTerm Java.VariableInitializer
variableInitializerArray = inject Java._VariableInitializer Java._VariableInitializer_arrayInitializer
