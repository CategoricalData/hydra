-- | Convenience layer over the generated Java syntax DSL.
-- Re-exports all generated DSL functions and adds custom helpers
-- for common Java AST construction patterns.

module Hydra.Dsl.Java.Helpers (
  module Hydra.Dsl.Java.Helpers,
  module Hydra.Dsl.Java.Syntax,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Java.Syntax hiding (
  -- Hide functions whose types have changed since the hand-written DSL was created.
  -- The hand-written versions are kept below for backward compatibility.
  arrayInitializer,
  classInstanceCreationExpression,
  lambdaParametersTuple,
  variableDeclaratorId)
import qualified Hydra.Java.Syntax as Java

import Prelude hiding (map)


-- Functions with signatures that differ from the generated versions.
-- These use the original types for backward compatibility with existing call sites.

arrayInitializer :: TypedTerm [Java.VariableInitializer] -> TypedTerm Java.ArrayInitializer
arrayInitializer = wrap Java._ArrayInitializer

classInstanceCreationExpression :: TypedTerm (Maybe Java.Expression) -> TypedTerm Java.UnqualifiedClassInstanceCreationExpression -> TypedTerm Java.ClassInstanceCreationExpression
classInstanceCreationExpression qual unqual = record Java._ClassInstanceCreationExpression [
  Java._ClassInstanceCreationExpression_qualifier>>: qual,
  Java._ClassInstanceCreationExpression_expression>>: unqual]

lambdaParametersTuple :: TypedTerm [Java.FormalParameter] -> TypedTerm Java.LambdaParameters
lambdaParametersTuple = inject Java._LambdaParameters Java._LambdaParameters_tuple

variableDeclaratorId :: TypedTerm Java.Identifier -> TypedTerm (Maybe Java.Dims) -> TypedTerm Java.VariableDeclaratorId
variableDeclaratorId ident dims_ = record Java._VariableDeclaratorId [
  Java._VariableDeclaratorId_identifier>>: ident,
  Java._VariableDeclaratorId_dims>>: dims_]


-- Custom helpers: expression type conversions

literalToPrimary :: TypedTerm Java.Literal -> TypedTerm Java.Primary
literalToPrimary lit = primaryNoNewArray (primaryLiteral lit)

literalToExpression :: TypedTerm Java.Literal -> TypedTerm Java.Expression
literalToExpression lit = primaryToExpression (literalToPrimary lit)

literalToRelationalExpression :: TypedTerm Java.Literal -> TypedTerm Java.RelationalExpression
literalToRelationalExpression lit =
  relationalExpressionSimple
    (shiftExpressionUnary
      (additiveExpressionUnary
        (multiplicativeExpressionUnary
          (unaryExpressionOther
            (unaryExpressionNotPlusMinusPostfix
              (postfixExpressionPrimary (literalToPrimary lit)))))))

literalToMultiplicativeExpression :: TypedTerm Java.Literal -> TypedTerm Java.MultiplicativeExpression
literalToMultiplicativeExpression lit =
  multiplicativeExpressionUnary
    (unaryExpressionOther
      (unaryExpressionNotPlusMinusPostfix
        (postfixExpressionPrimary (literalToPrimary lit))))

primaryToExpression :: TypedTerm Java.Primary -> TypedTerm Java.Expression
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

postfixToExpression :: TypedTerm Java.PostfixExpression -> TypedTerm Java.Expression
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

postfixToInclusiveOr :: TypedTerm Java.PostfixExpression -> TypedTerm Java.InclusiveOrExpression
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

postfixToRelationalExpression :: TypedTerm Java.PostfixExpression -> TypedTerm Java.RelationalExpression
postfixToRelationalExpression pe =
  relationalExpressionSimple
    (shiftExpressionUnary
      (additiveExpressionUnary
        (multiplicativeExpressionUnary
          (unaryExpressionOther
            (unaryExpressionNotPlusMinusPostfix pe)))))

unaryToExpression :: TypedTerm Java.UnaryExpression -> TypedTerm Java.Expression
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

additiveToExpression :: TypedTerm Java.AdditiveExpression -> TypedTerm Java.Expression
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

equalityToExpression :: TypedTerm Java.EqualityExpression -> TypedTerm Java.Expression
equalityToExpression ee = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [
        conditionalAndExpression (list [
          equalityToInclusiveOr ee])]))))

equalityToInclusiveOr :: TypedTerm Java.EqualityExpression -> TypedTerm Java.InclusiveOrExpression
equalityToInclusiveOr ee = inclusiveOrExpression (list [
  exclusiveOrExpression (list [
    andExpression (list [ee])])])

conditionalAndToExpression :: TypedTerm Java.ConditionalAndExpression -> TypedTerm Java.Expression
conditionalAndToExpression cae = expressionAssignment
  (assignmentExpressionConditional
    (conditionalExpressionSimple
      (conditionalOrExpression (list [cae]))))

castExpressionToExpression :: TypedTerm Java.CastExpression -> TypedTerm Java.Expression
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

expressionNameToExpression :: TypedTerm Java.ExpressionName -> TypedTerm Java.Expression
expressionNameToExpression en = postfixToExpression (postfixExpressionName en)

identifierToExpression :: TypedTerm Java.Identifier -> TypedTerm Java.Expression
identifierToExpression id_ = expressionNameToExpression (expressionName nothing id_)

identifierToRelationalExpression :: TypedTerm Java.Identifier -> TypedTerm Java.RelationalExpression
identifierToRelationalExpression id_ =
  postfixToRelationalExpression (postfixExpressionName (expressionName nothing id_))

identifierToUnary :: TypedTerm Java.Identifier -> TypedTerm Java.UnaryExpression
identifierToUnary id_ = unaryExpressionOther
  (unaryExpressionNotPlusMinusPostfix
    (postfixExpressionName (expressionName nothing id_)))

fieldAccessToExpression :: TypedTerm Java.FieldAccess -> TypedTerm Java.Expression
fieldAccessToExpression fa = primaryToExpression (primaryNoNewArray (primaryFieldAccess fa))

methodInvocationToExpression :: TypedTerm Java.MethodInvocation -> TypedTerm Java.Expression
methodInvocationToExpression mi = primaryToExpression (primaryNoNewArray (primaryMethodInvocation mi))

methodInvocationToPrimary :: TypedTerm Java.MethodInvocation -> TypedTerm Java.Primary
methodInvocationToPrimary mi = primaryNoNewArray (primaryMethodInvocation mi)

methodInvocationToPostfix :: TypedTerm Java.MethodInvocation -> TypedTerm Java.PostfixExpression
methodInvocationToPostfix mi = postfixExpressionPrimary (primaryNoNewArray (primaryMethodInvocation mi))

methodInvocationToStatement :: TypedTerm Java.MethodInvocation -> TypedTerm Java.Statement
methodInvocationToStatement mi = statementWithoutTrailing
  (stmtExpression (expressionStatement (stmtExprMethodInvocation mi)))

expressionToPrimary :: TypedTerm Java.Expression -> TypedTerm Java.Primary
expressionToPrimary e = primaryNoNewArray (primaryParens e)

expressionToUnary :: TypedTerm Java.Expression -> TypedTerm Java.UnaryExpression
expressionToUnary e = unaryExpressionOther
  (unaryExpressionNotPlusMinusPostfix
    (postfixExpressionPrimary (expressionToPrimary e)))

primaryToUnary :: TypedTerm Java.UnaryExpression -> TypedTerm Java.Primary
primaryToUnary ue = primaryNoNewArray (primaryParens (unaryToExpression ue))

relationalToUnary :: TypedTerm Java.RelationalExpression -> TypedTerm Java.UnaryExpression
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

additiveExpressionBinary :: TypedTerm Java.AdditiveExpression -> TypedTerm Java.MultiplicativeExpression -> TypedTerm Java.AdditiveExpression_Binary
additiveExpressionBinary lhs rhs = record Java._AdditiveExpression_Binary [
  Java._AdditiveExpression_Binary_lhs>>: lhs,
  Java._AdditiveExpression_Binary_rhs>>: rhs]

equalityExpressionBinary :: TypedTerm Java.EqualityExpression -> TypedTerm Java.RelationalExpression -> TypedTerm Java.EqualityExpression_Binary
equalityExpressionBinary lhs rhs = record Java._EqualityExpression_Binary [
  Java._EqualityExpression_Binary_lhs>>: lhs,
  Java._EqualityExpression_Binary_rhs>>: rhs]

multiplicativeExpressionBinary :: TypedTerm Java.MultiplicativeExpression -> TypedTerm Java.UnaryExpression -> TypedTerm Java.MultiplicativeExpression_Binary
multiplicativeExpressionBinary lhs rhs = record Java._MultiplicativeExpression_Binary [
  Java._MultiplicativeExpression_Binary_lhs>>: lhs,
  Java._MultiplicativeExpression_Binary_rhs>>: rhs]


-- Custom helpers: primary constructors

primaryLiteral :: TypedTerm Java.Literal -> TypedTerm Java.PrimaryNoNewArrayExpression
primaryLiteral = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_literal

primaryParens :: TypedTerm Java.Expression -> TypedTerm Java.PrimaryNoNewArrayExpression
primaryParens = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_parens

primaryFieldAccess :: TypedTerm Java.FieldAccess -> TypedTerm Java.PrimaryNoNewArrayExpression
primaryFieldAccess = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_fieldAccess

primaryMethodInvocation :: TypedTerm Java.MethodInvocation -> TypedTerm Java.PrimaryNoNewArrayExpression
primaryMethodInvocation = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_methodInvocation

primaryThis :: TypedTerm Java.PrimaryNoNewArrayExpression
primaryThis = injectUnit Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_this

primaryClassInstance :: TypedTerm Java.ClassInstanceCreationExpression -> TypedTerm Java.PrimaryNoNewArrayExpression
primaryClassInstance = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_classInstance

primaryArrayAccess :: TypedTerm Java.ArrayAccess -> TypedTerm Java.PrimaryNoNewArrayExpression
primaryArrayAccess = inject Java._PrimaryNoNewArrayExpression Java._PrimaryNoNewArrayExpression_arrayAccess


-- Custom helpers: statement constructors

stmtExpression :: TypedTerm Java.ExpressionStatement -> TypedTerm Java.StatementWithoutTrailingSubstatement
stmtExpression = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_expression

stmtReturn :: TypedTerm Java.ReturnStatement -> TypedTerm Java.StatementWithoutTrailingSubstatement
stmtReturn = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_return

stmtThrow :: TypedTerm Java.ThrowStatement -> TypedTerm Java.StatementWithoutTrailingSubstatement
stmtThrow = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_throw

stmtEmpty :: TypedTerm Java.StatementWithoutTrailingSubstatement
stmtEmpty = injectUnit Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_empty

stmtBlock :: TypedTerm Java.Block -> TypedTerm Java.StatementWithoutTrailingSubstatement
stmtBlock = inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_block

stmtExprAssignment :: TypedTerm Java.Assignment -> TypedTerm Java.StatementExpression
stmtExprAssignment = inject Java._StatementExpression Java._StatementExpression_assignment

stmtExprMethodInvocation :: TypedTerm Java.MethodInvocation -> TypedTerm Java.StatementExpression
stmtExprMethodInvocation = inject Java._StatementExpression Java._StatementExpression_methodInvocation

stmtExprClassInstance :: TypedTerm Java.ClassInstanceCreationExpression -> TypedTerm Java.StatementExpression
stmtExprClassInstance = inject Java._StatementExpression Java._StatementExpression_classInstanceCreation


-- Custom helpers: other constructors

arrayCreationExpressionPrimitiveArray_ :: TypedTerm Java.PrimitiveTypeWithAnnotations -> TypedTerm [Java.Dims] -> TypedTerm Java.ArrayInitializer -> TypedTerm Java.ArrayCreationExpressionWithInitializer_Primitive
arrayCreationExpressionPrimitiveArray_ pt dims_ arr = record Java._ArrayCreationExpressionWithInitializer_Primitive [
  Java._ArrayCreationExpressionWithInitializer_Primitive_type>>: pt,
  Java._ArrayCreationExpressionWithInitializer_Primitive_dims>>: dims_,
  Java._ArrayCreationExpressionWithInitializer_Primitive_array>>: arr]

arrayTypeVariantPrimitive :: TypedTerm Java.PrimitiveTypeWithAnnotations -> TypedTerm Java.ArrayType_Variant
arrayTypeVariantPrimitive = inject Java._ArrayType_Variant Java._ArrayType_Variant_primitive

castExpressionNotPlusMinus_ :: TypedTerm Java.CastExpression_RefAndBounds -> TypedTerm Java.UnaryExpressionNotPlusMinus -> TypedTerm Java.CastExpression_NotPlusMinus
castExpressionNotPlusMinus_ rb expr = record Java._CastExpression_NotPlusMinus [
  Java._CastExpression_NotPlusMinus_refAndBounds>>: rb,
  Java._CastExpression_NotPlusMinus_expression>>: expr]

castExpressionPrimitive_ :: TypedTerm Java.PrimitiveTypeWithAnnotations -> TypedTerm Java.UnaryExpression -> TypedTerm Java.CastExpression_Primitive
castExpressionPrimitive_ pt expr = record Java._CastExpression_Primitive [
  Java._CastExpression_Primitive_type>>: pt,
  Java._CastExpression_Primitive_expression>>: expr]

castExpressionRefAndBounds :: TypedTerm Java.ReferenceType -> TypedTerm [Java.AdditionalBound] -> TypedTerm Java.CastExpression_RefAndBounds
castExpressionRefAndBounds rt bounds = record Java._CastExpression_RefAndBounds [
  Java._CastExpression_RefAndBounds_type>>: rt,
  Java._CastExpression_RefAndBounds_bounds>>: bounds]

fieldAccessQualifierPrimary :: TypedTerm Java.Primary -> TypedTerm Java.FieldAccess_Qualifier
fieldAccessQualifierPrimary = inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary

relationalExpressionInstanceOf :: TypedTerm Java.InstanceofExpression -> TypedTerm Java.RelationalExpression
relationalExpressionInstanceOf = inject Java._RelationalExpression Java._RelationalExpression_instanceofExpression

relationalExpressionInstanceOf_ :: TypedTerm Java.RelationalExpression -> TypedTerm Java.ReferenceType -> TypedTerm Java.InstanceofExpression
relationalExpressionInstanceOf_ expr rt = record Java._InstanceofExpression [
  Java._InstanceofExpression_lhs>>: expr,
  Java._InstanceofExpression_rhs>>: inject Java._InstanceofExpression_Rhs Java._InstanceofExpression_Rhs_referenceType rt]

methodInvocation_ :: TypedTerm Java.MethodInvocation_Header -> TypedTerm [Java.Expression] -> TypedTerm Java.MethodInvocation
methodInvocation_ header args = record Java._MethodInvocation [
  Java._MethodInvocation_header>>: header,
  Java._MethodInvocation_arguments>>: args]

methodInvocationHeaderSimple :: TypedTerm Java.MethodName -> TypedTerm Java.MethodInvocation_Header
methodInvocationHeaderSimple = inject Java._MethodInvocation_Header Java._MethodInvocation_Header_simple

methodInvocationHeaderComplex :: TypedTerm Java.MethodInvocation_Complex -> TypedTerm Java.MethodInvocation_Header
methodInvocationHeaderComplex = inject Java._MethodInvocation_Header Java._MethodInvocation_Header_complex

methodInvocationComplex :: TypedTerm Java.MethodInvocation_Variant -> TypedTerm [Java.TypeArgument] -> TypedTerm Java.Identifier -> TypedTerm Java.MethodInvocation_Complex
methodInvocationComplex variant targs ident = record Java._MethodInvocation_Complex [
  Java._MethodInvocation_Complex_variant>>: variant,
  Java._MethodInvocation_Complex_typeArguments>>: targs,
  Java._MethodInvocation_Complex_identifier>>: ident]

methodInvocationVariantExpression :: TypedTerm Java.ExpressionName -> TypedTerm Java.MethodInvocation_Variant
methodInvocationVariantExpression = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_expression

methodInvocationVariantPrimary :: TypedTerm Java.Primary -> TypedTerm Java.MethodInvocation_Variant
methodInvocationVariantPrimary = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_primary

methodInvocationVariantType :: TypedTerm Java.TypeName -> TypedTerm Java.MethodInvocation_Variant
methodInvocationVariantType = inject Java._MethodInvocation_Variant Java._MethodInvocation_Variant_type

methodDeclaration_ :: TypedTerm [Java.Annotation] -> TypedTerm [Java.MethodModifier] -> TypedTerm Java.MethodHeader -> TypedTerm Java.MethodBody -> TypedTerm Java.MethodDeclaration
methodDeclaration_ anns mods header body = record Java._MethodDeclaration [
  Java._MethodDeclaration_annotations>>: anns,
  Java._MethodDeclaration_modifiers>>: mods,
  Java._MethodDeclaration_header>>: header,
  Java._MethodDeclaration_body>>: body]

interfaceMethodDeclaration_ :: TypedTerm [Java.InterfaceMethodModifier] -> TypedTerm Java.MethodHeader -> TypedTerm Java.MethodBody -> TypedTerm Java.InterfaceMethodDeclaration
interfaceMethodDeclaration_ mods header body = record Java._InterfaceMethodDeclaration [
  Java._InterfaceMethodDeclaration_modifiers>>: mods,
  Java._InterfaceMethodDeclaration_header>>: header,
  Java._InterfaceMethodDeclaration_body>>: body]

formalParameterSimple_ :: TypedTerm [Java.VariableModifier] -> TypedTerm Java.UnannType -> TypedTerm Java.VariableDeclaratorId -> TypedTerm Java.FormalParameter_Simple
formalParameterSimple_ mods ut vid = record Java._FormalParameter_Simple [
  Java._FormalParameter_Simple_modifiers>>: mods,
  Java._FormalParameter_Simple_type>>: ut,
  Java._FormalParameter_Simple_id>>: vid]

classBodyDeclClassMember :: TypedTerm Java.ClassMemberDeclaration -> TypedTerm Java.ClassBodyDeclaration
classBodyDeclClassMember = inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_classMember

classBodyDeclConstructor :: TypedTerm Java.ConstructorDeclaration -> TypedTerm Java.ClassBodyDeclaration
classBodyDeclConstructor = inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_constructorDeclaration

classMemberDeclField :: TypedTerm Java.FieldDeclaration -> TypedTerm Java.ClassMemberDeclaration
classMemberDeclField = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_field

classMemberDeclMethod :: TypedTerm Java.MethodDeclaration -> TypedTerm Java.ClassMemberDeclaration
classMemberDeclMethod = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_method

classMemberDeclClass :: TypedTerm Java.ClassDeclaration -> TypedTerm Java.ClassMemberDeclaration
classMemberDeclClass = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_class

classMemberDeclInterface :: TypedTerm Java.InterfaceDeclaration -> TypedTerm Java.ClassMemberDeclaration
classMemberDeclInterface = inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_interface

interfaceMemberDeclConstant :: TypedTerm Java.ConstantDeclaration -> TypedTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclConstant = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_constant

interfaceMemberDeclInterfaceMethod :: TypedTerm Java.InterfaceMethodDeclaration -> TypedTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclInterfaceMethod = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_interfaceMethod

interfaceMemberDeclClass :: TypedTerm Java.ClassDeclaration -> TypedTerm Java.InterfaceMemberDeclaration
interfaceMemberDeclClass = inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_class

elementValueConditional :: TypedTerm Java.ConditionalExpression -> TypedTerm Java.ElementValue
elementValueConditional = inject Java._ElementValue Java._ElementValue_conditionalExpression

elementValueArray :: TypedTerm Java.ElementValueArrayInitializer -> TypedTerm Java.ElementValue
elementValueArray = inject Java._ElementValue Java._ElementValue_elementValueArrayInitializer

variableInitializerArray :: TypedTerm Java.ArrayInitializer -> TypedTerm Java.VariableInitializer
variableInitializerArray = inject Java._VariableInitializer Java._VariableInitializer_arrayInitializer
