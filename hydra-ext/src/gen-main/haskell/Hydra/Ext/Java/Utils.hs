-- Note: this is an automatically generated file. Do not edit.

-- | Java utilities for constructing Java syntax trees

module Hydra.Ext.Java.Utils where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Java.Helpers as Helpers
import qualified Hydra.Ext.Java.Language as Language
import qualified Hydra.Ext.Java.Names as Names
import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

javaIdentifier :: (String -> Syntax.Identifier)
javaIdentifier s = (Syntax.Identifier (sanitizeJavaName s))

javaTypeIdentifier :: (String -> Syntax.TypeIdentifier)
javaTypeIdentifier s = (Syntax.TypeIdentifier (Syntax.Identifier s))

javaTypeName :: (Syntax.Identifier -> Syntax.TypeName)
javaTypeName id = Syntax.TypeName {
  Syntax.typeNameIdentifier = (Syntax.TypeIdentifier id),
  Syntax.typeNameQualifier = Nothing}

javaDeclName :: (Core.Name -> Syntax.TypeIdentifier)
javaDeclName name = (Syntax.TypeIdentifier (javaVariableName name))

javaVariableName :: (Core.Name -> Syntax.Identifier)
javaVariableName name = (javaIdentifier (Names_.localNameOf name))

javaVariableDeclaratorId :: (Syntax.Identifier -> Syntax.VariableDeclaratorId)
javaVariableDeclaratorId id = Syntax.VariableDeclaratorId {
  Syntax.variableDeclaratorIdIdentifier = id,
  Syntax.variableDeclaratorIdDims = Nothing}

javaVariableDeclarator :: (Syntax.Identifier -> Maybe Syntax.VariableInitializer -> Syntax.VariableDeclarator)
javaVariableDeclarator id minit = Syntax.VariableDeclarator {
  Syntax.variableDeclaratorId = (javaVariableDeclaratorId id),
  Syntax.variableDeclaratorInitializer = minit}

javaBoolean :: (Bool -> Syntax.Literal)
javaBoolean b = (Syntax.LiteralBoolean b)

javaInt :: (Integer -> Syntax.Literal)
javaInt i = (Syntax.LiteralInteger (Syntax.IntegerLiteral i))

javaString :: (String -> Syntax.Literal)
javaString s = (Syntax.LiteralString (Syntax.StringLiteral s))

javaLiteralToJavaPrimary :: (Syntax.Literal -> Syntax.Primary)
javaLiteralToJavaPrimary lit = (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayLiteral lit))

javaExpressionToJavaPrimary :: (Syntax.Expression -> Syntax.Primary)
javaExpressionToJavaPrimary e = (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayParens e))

javaPrimaryToJavaUnaryExpression :: (Syntax.Primary -> Syntax.UnaryExpression)
javaPrimaryToJavaUnaryExpression p = (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary p)))

javaPrimaryToJavaExpression :: (Syntax.Primary -> Syntax.Expression)
javaPrimaryToJavaExpression p = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary p)))))))]]]]]))))

javaPostfixExpressionToJavaUnaryExpression :: (Syntax.PostfixExpression -> Syntax.UnaryExpression)
javaPostfixExpressionToJavaUnaryExpression pe = (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe))

javaPostfixExpressionToJavaExpression :: (Syntax.PostfixExpression -> Syntax.Expression)
javaPostfixExpressionToJavaExpression pe = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe))))))]]]]]))))

javaPostfixExpressionToJavaRelationalExpression :: (Syntax.PostfixExpression -> Syntax.RelationalExpression)
javaPostfixExpressionToJavaRelationalExpression pe = (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe))))))

javaUnaryExpressionToJavaRelationalExpression :: (Syntax.UnaryExpression -> Syntax.RelationalExpression)
javaUnaryExpressionToJavaRelationalExpression ue = (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary ue))))

javaUnaryExpressionToJavaExpression :: (Syntax.UnaryExpression -> Syntax.Expression)
javaUnaryExpressionToJavaExpression ue = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary ue))))]]]]]))))

javaRelationalExpressionToJavaExpression :: (Syntax.RelationalExpression -> Syntax.Expression)
javaRelationalExpressionToJavaExpression re = (javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionUnary re))

javaRelationalExpressionToJavaUnaryExpression :: (Syntax.RelationalExpression -> Syntax.UnaryExpression)
javaRelationalExpressionToJavaUnaryExpression re = (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayParens (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary re]]]]])))))))))

javaMultiplicativeExpressionToJavaRelationalExpression :: (Syntax.MultiplicativeExpression -> Syntax.RelationalExpression)
javaMultiplicativeExpressionToJavaRelationalExpression me = (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary me)))

javaLiteralToJavaMultiplicativeExpression :: (Syntax.Literal -> Syntax.MultiplicativeExpression)
javaLiteralToJavaMultiplicativeExpression lit = (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayLiteral lit))))))

javaLiteralToJavaRelationalExpression :: (Syntax.Literal -> Syntax.RelationalExpression)
javaLiteralToJavaRelationalExpression lit = (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayLiteral lit)))))))))

javaLiteralToJavaExpression :: (Syntax.Literal -> Syntax.Expression)
javaLiteralToJavaExpression lit = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayLiteral lit)))))))))]]]]]))))

javaIdentifierToJavaExpressionName :: (Syntax.Identifier -> Syntax.ExpressionName)
javaIdentifierToJavaExpressionName id = Syntax.ExpressionName {
  Syntax.expressionNameQualifier = Nothing,
  Syntax.expressionNameIdentifier = id}

javaIdentifierToJavaExpression :: (Syntax.Identifier -> Syntax.Expression)
javaIdentifierToJavaExpression id = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (Syntax.ExpressionName {
            Syntax.expressionNameQualifier = Nothing,
            Syntax.expressionNameIdentifier = id}))))))))]]]]]))))

javaIdentifierToJavaRelationalExpression :: (Syntax.Identifier -> Syntax.RelationalExpression)
javaIdentifierToJavaRelationalExpression id = (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (Syntax.ExpressionName {
  Syntax.expressionNameQualifier = Nothing,
  Syntax.expressionNameIdentifier = id}))))))))

javaIdentifierToJavaUnaryExpression :: (Syntax.Identifier -> Syntax.UnaryExpression)
javaIdentifierToJavaUnaryExpression id = (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (Syntax.ExpressionName {
  Syntax.expressionNameQualifier = Nothing,
  Syntax.expressionNameIdentifier = id}))))

javaExpressionNameToJavaExpression :: (Syntax.ExpressionName -> Syntax.Expression)
javaExpressionNameToJavaExpression en = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName en)))))))]]]]]))))

javaFieldAccessToJavaExpression :: (Syntax.FieldAccess -> Syntax.Expression)
javaFieldAccessToJavaExpression fa = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayFieldAccess fa)))))))))]]]]]))))

javaCastExpressionToJavaExpression :: (Syntax.CastExpression -> Syntax.Expression)
javaCastExpressionToJavaExpression ce = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusCast ce))))))]]]]]))))

javaMethodInvocationToJavaPrimary :: (Syntax.MethodInvocation -> Syntax.Primary)
javaMethodInvocationToJavaPrimary mi = (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayMethodInvocation mi))

javaMethodInvocationToJavaExpression :: (Syntax.MethodInvocation -> Syntax.Expression)
javaMethodInvocationToJavaExpression mi = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayMethodInvocation mi)))))))))]]]]]))))

javaMethodInvocationToJavaPostfixExpression :: (Syntax.MethodInvocation -> Syntax.PostfixExpression)
javaMethodInvocationToJavaPostfixExpression mi = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayMethodInvocation mi)))

javaMethodInvocationToJavaStatement :: (Syntax.MethodInvocation -> Syntax.Statement)
javaMethodInvocationToJavaStatement mi = (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementExpression (Syntax.ExpressionStatement (Syntax.StatementExpressionMethodInvocation mi))))

javaConditionalAndExpressionToJavaExpression :: (Syntax.ConditionalAndExpression -> Syntax.Expression)
javaConditionalAndExpressionToJavaExpression cae = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  cae]))))

javaEqualityExpressionToJavaInclusiveOrExpression :: (Syntax.EqualityExpression -> Syntax.InclusiveOrExpression)
javaEqualityExpressionToJavaInclusiveOrExpression ee = (Syntax.InclusiveOrExpression [
  Syntax.ExclusiveOrExpression [
    Syntax.AndExpression [
      ee]]])

javaEqualityExpressionToJavaExpression :: (Syntax.EqualityExpression -> Syntax.Expression)
javaEqualityExpressionToJavaExpression ee = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          ee]]]]]))))

javaPostfixExpressionToJavaEqualityExpression :: (Syntax.PostfixExpression -> Syntax.EqualityExpression)
javaPostfixExpressionToJavaEqualityExpression pe = (Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe)))))))

javaPostfixExpressionToJavaInclusiveOrExpression :: (Syntax.PostfixExpression -> Syntax.InclusiveOrExpression)
javaPostfixExpressionToJavaInclusiveOrExpression pe = (Syntax.InclusiveOrExpression [
  Syntax.ExclusiveOrExpression [
    Syntax.AndExpression [
      Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe))))))]]])

javaAdditiveExpressionToJavaExpression :: (Syntax.AdditiveExpression -> Syntax.Expression)
javaAdditiveExpressionToJavaExpression ae = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary ae))]]]]]))))

javaExpressionToJavaUnaryExpression :: (Syntax.Expression -> Syntax.UnaryExpression)
javaExpressionToJavaUnaryExpression e = (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayParens e)))))

javaPrimitiveTypeToJavaType :: (Syntax.PrimitiveType -> Syntax.Type)
javaPrimitiveTypeToJavaType pt = (Syntax.TypePrimitive (Syntax.PrimitiveTypeWithAnnotations {
  Syntax.primitiveTypeWithAnnotationsType = pt,
  Syntax.primitiveTypeWithAnnotationsAnnotations = []}))

javaClassTypeToJavaType :: (Syntax.ClassType -> Syntax.Type)
javaClassTypeToJavaType ct = (Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass ct)))

javaTypeVariableToType :: (Syntax.TypeVariable -> Syntax.Type)
javaTypeVariableToType tv = (Syntax.TypeReference (Syntax.ReferenceTypeVariable tv))

javaRefType :: ([Syntax.ReferenceType] -> Maybe Syntax.PackageName -> String -> Syntax.Type)
javaRefType args pkg id = (Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (javaClassType args pkg id))))

javaClassType :: ([Syntax.ReferenceType] -> Maybe Syntax.PackageName -> String -> Syntax.ClassType)
javaClassType args pkg id =  
  let qual = (Maybes.cases pkg Syntax.ClassTypeQualifierNone (\p -> Syntax.ClassTypeQualifierPackage p)) 
      targs = (Lists.map (\rt -> Syntax.TypeArgumentReference rt) args)
  in Syntax.ClassType {
    Syntax.classTypeAnnotations = [],
    Syntax.classTypeQualifier = qual,
    Syntax.classTypeIdentifier = (javaTypeIdentifier id),
    Syntax.classTypeArguments = targs}

javaTypeVariable :: (String -> Syntax.ReferenceType)
javaTypeVariable v = (Syntax.ReferenceTypeVariable (Syntax.TypeVariable {
  Syntax.typeVariableAnnotations = [],
  Syntax.typeVariableIdentifier = (javaTypeIdentifier (Formatting.capitalize v))}))

javaBooleanType :: Syntax.Type
javaBooleanType = (javaPrimitiveTypeToJavaType Syntax.PrimitiveTypeBoolean)

javaIntType :: Syntax.Type
javaIntType = (javaPrimitiveTypeToJavaType (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeInt)))

javaBooleanExpression :: (Bool -> Syntax.Expression)
javaBooleanExpression b = (javaPrimaryToJavaExpression (javaLiteralToJavaPrimary (javaBoolean b)))

javaIntExpression :: (Integer -> Syntax.Expression)
javaIntExpression i = (javaPrimaryToJavaExpression (javaLiteralToJavaPrimary (javaInt i)))

javaCastExpression :: (Syntax.ReferenceType -> Syntax.UnaryExpression -> Syntax.CastExpression)
javaCastExpression rt expr = (Syntax.CastExpressionNotPlusMinus (Syntax.CastExpression_NotPlusMinus {
  Syntax.castExpression_NotPlusMinusRefAndBounds = Syntax.CastExpression_RefAndBounds {
    Syntax.castExpression_RefAndBoundsType = rt,
    Syntax.castExpression_RefAndBoundsBounds = []},
  Syntax.castExpression_NotPlusMinusExpression = expr}))

javaCastPrimitive :: (Syntax.PrimitiveType -> Syntax.UnaryExpression -> Syntax.CastExpression)
javaCastPrimitive pt expr = (Syntax.CastExpressionPrimitive (Syntax.CastExpression_Primitive {
  Syntax.castExpression_PrimitiveType = Syntax.PrimitiveTypeWithAnnotations {
    Syntax.primitiveTypeWithAnnotationsType = pt,
    Syntax.primitiveTypeWithAnnotationsAnnotations = []},
  Syntax.castExpression_PrimitiveExpression = expr}))

javaReturnStatement :: (Maybe Syntax.Expression -> Syntax.Statement)
javaReturnStatement mex = (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementReturn (Syntax.ReturnStatement mex)))

javaThrowStatement :: (Syntax.Expression -> Syntax.Statement)
javaThrowStatement e = (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementThrow (Syntax.ThrowStatement e)))

javaEmptyStatement :: Syntax.Statement
javaEmptyStatement = (Syntax.StatementWithoutTrailing Syntax.StatementWithoutTrailingSubstatementEmpty)

javaAssignmentStatement :: (Syntax.LeftHandSide -> Syntax.Expression -> Syntax.Statement)
javaAssignmentStatement lhs rhs = (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementExpression (Syntax.ExpressionStatement (Syntax.StatementExpressionAssignment (Syntax.Assignment {
  Syntax.assignmentLhs = lhs,
  Syntax.assignmentOp = Syntax.AssignmentOperatorSimple,
  Syntax.assignmentExpression = rhs})))))

javaStatementsToBlock :: ([Syntax.Statement] -> Syntax.Block)
javaStatementsToBlock stmts = (Syntax.Block (Lists.map (\s -> Syntax.BlockStatementStatement s) stmts))

javaLambda :: (Core.Name -> Syntax.Expression -> Syntax.Expression)
javaLambda v body = (Syntax.ExpressionLambda (Syntax.LambdaExpression {
  Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersSingle (variableToJavaIdentifier v)),
  Syntax.lambdaExpressionBody = (Syntax.LambdaBodyExpression body)}))

javaLambdaFromBlock :: (Core.Name -> Syntax.Block -> Syntax.Expression)
javaLambdaFromBlock v block = (Syntax.ExpressionLambda (Syntax.LambdaExpression {
  Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersSingle (variableToJavaIdentifier v)),
  Syntax.lambdaExpressionBody = (Syntax.LambdaBodyBlock block)}))

javaMethodBody :: (Maybe [Syntax.BlockStatement] -> Syntax.MethodBody)
javaMethodBody mstmts = (Maybes.cases mstmts Syntax.MethodBodyNone (\stmts -> Syntax.MethodBodyBlock (Syntax.Block stmts)))

javaMethodHeader :: ([Syntax.TypeParameter] -> String -> [Syntax.FormalParameter] -> Syntax.Result -> Syntax.MethodHeader)
javaMethodHeader tparams methodName params result = Syntax.MethodHeader {
  Syntax.methodHeaderParameters = tparams,
  Syntax.methodHeaderResult = result,
  Syntax.methodHeaderDeclarator = Syntax.MethodDeclarator {
    Syntax.methodDeclaratorIdentifier = (Syntax.Identifier methodName),
    Syntax.methodDeclaratorReceiverParameter = Nothing,
    Syntax.methodDeclaratorFormalParameters = params},
  Syntax.methodHeaderThrows = Nothing}

javaMethodDeclarationToJavaClassBodyDeclaration :: (Syntax.MethodDeclaration -> Syntax.ClassBodyDeclaration)
javaMethodDeclarationToJavaClassBodyDeclaration md = (Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationMethod md))

javaInterfaceDeclarationToJavaClassBodyDeclaration :: (Syntax.NormalInterfaceDeclaration -> Syntax.ClassBodyDeclaration)
javaInterfaceDeclarationToJavaClassBodyDeclaration nid = (Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationInterface (Syntax.InterfaceDeclarationNormalInterface nid)))

javaMemberField :: ([Syntax.FieldModifier] -> Syntax.Type -> Syntax.VariableDeclarator -> Syntax.ClassBodyDeclaration)
javaMemberField mods jt v = (Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationField (Syntax.FieldDeclaration {
  Syntax.fieldDeclarationModifiers = mods,
  Syntax.fieldDeclarationUnannType = (Syntax.UnannType jt),
  Syntax.fieldDeclarationVariableDeclarators = [
    v]})))

javaTypeToJavaFormalParameter :: (Syntax.Type -> Core.Name -> Syntax.FormalParameter)
javaTypeToJavaFormalParameter jt fname = (Syntax.FormalParameterSimple (Syntax.FormalParameter_Simple {
  Syntax.formalParameter_SimpleModifiers = [],
  Syntax.formalParameter_SimpleType = (Syntax.UnannType jt),
  Syntax.formalParameter_SimpleId = (fieldNameToJavaVariableDeclaratorId fname)}))

javaTypeToJavaResult :: (Syntax.Type -> Syntax.Result)
javaTypeToJavaResult jt = (Syntax.ResultType (Syntax.UnannType jt))

javaTypeToJavaTypeArgument :: (Syntax.Type -> Syntax.TypeArgument)
javaTypeToJavaTypeArgument t = ((\x -> case x of
  Syntax.TypeReference v1 -> (Syntax.TypeArgumentReference v1)
  Syntax.TypePrimitive _ -> (Syntax.TypeArgumentWildcard (Syntax.Wildcard {
    Syntax.wildcardAnnotations = [],
    Syntax.wildcardWildcard = Nothing}))) t)

referenceTypeToResult :: (Syntax.ReferenceType -> Syntax.Result)
referenceTypeToResult rt = (javaTypeToJavaResult (Syntax.TypeReference rt))

javaConstructorName :: (Syntax.Identifier -> Maybe Syntax.TypeArgumentsOrDiamond -> Syntax.ClassOrInterfaceTypeToInstantiate)
javaConstructorName id targs = Syntax.ClassOrInterfaceTypeToInstantiate {
  Syntax.classOrInterfaceTypeToInstantiateIdentifiers = [
    Syntax.AnnotatedIdentifier {
      Syntax.annotatedIdentifierAnnotations = [],
      Syntax.annotatedIdentifierIdentifier = id}],
  Syntax.classOrInterfaceTypeToInstantiateTypeArguments = targs}

javaConstructorCall :: (Syntax.ClassOrInterfaceTypeToInstantiate -> [Syntax.Expression] -> Maybe Syntax.ClassBody -> Syntax.Expression)
javaConstructorCall ci args mbody = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ (Syntax.PrimaryNoNewArrayClassInstance (Syntax.ClassInstanceCreationExpression {
            Syntax.classInstanceCreationExpressionQualifier = Nothing,
            Syntax.classInstanceCreationExpressionExpression = Syntax.UnqualifiedClassInstanceCreationExpression {
              Syntax.unqualifiedClassInstanceCreationExpressionTypeArguments = [],
              Syntax.unqualifiedClassInstanceCreationExpressionClassOrInterface = ci,
              Syntax.unqualifiedClassInstanceCreationExpressionArguments = args,
              Syntax.unqualifiedClassInstanceCreationExpressionBody = mbody}}))))))))))]]]]]))))

javaThis :: Syntax.Expression
javaThis = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray_ Syntax.PrimaryNoNewArrayThis))))))))]]]]]))))

javaTypeParameter :: (String -> Syntax.TypeParameter)
javaTypeParameter v = Syntax.TypeParameter {
  Syntax.typeParameterModifiers = [],
  Syntax.typeParameterIdentifier = (javaTypeIdentifier v),
  Syntax.typeParameterBound = Nothing}

javaTypeIdentifierToJavaTypeArgument :: (Syntax.TypeIdentifier -> Syntax.TypeArgument)
javaTypeIdentifierToJavaTypeArgument id = (Syntax.TypeArgumentReference (Syntax.ReferenceTypeVariable (Syntax.TypeVariable {
  Syntax.typeVariableAnnotations = [],
  Syntax.typeVariableIdentifier = id})))

typeParameterToTypeArgument :: (Syntax.TypeParameter -> Syntax.TypeArgument)
typeParameterToTypeArgument tp = (javaTypeIdentifierToJavaTypeArgument (Syntax.typeParameterIdentifier tp))

typeParameterToReferenceType :: (Syntax.TypeParameter -> Syntax.ReferenceType)
typeParameterToReferenceType tp = (javaTypeVariable (Syntax.unIdentifier (Syntax.unTypeIdentifier (Syntax.typeParameterIdentifier tp))))

fieldNameToJavaIdentifier :: (Core.Name -> Syntax.Identifier)
fieldNameToJavaIdentifier fname = (javaIdentifier (Core.unName fname))

fieldNameToJavaExpression :: (Core.Name -> Syntax.Expression)
fieldNameToJavaExpression fname = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
  Syntax.ConditionalAndExpression [
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (javaIdentifierToJavaExpressionName (fieldNameToJavaIdentifier fname)))))))))]]]]]))))

fieldNameToJavaVariableDeclaratorId :: (Core.Name -> Syntax.VariableDeclaratorId)
fieldNameToJavaVariableDeclaratorId fname = (javaVariableDeclaratorId (javaIdentifier (Core.unName fname)))

fieldNameToJavaVariableDeclarator :: (Core.Name -> Syntax.VariableDeclarator)
fieldNameToJavaVariableDeclarator fname = (javaVariableDeclarator (javaIdentifier (Core.unName fname)) Nothing)

fieldExpression :: (Syntax.Identifier -> Syntax.Identifier -> Syntax.ExpressionName)
fieldExpression varId fieldId = Syntax.ExpressionName {
  Syntax.expressionNameQualifier = (Just (Syntax.AmbiguousName [
    varId])),
  Syntax.expressionNameIdentifier = fieldId}

variableToJavaIdentifier :: (Core.Name -> Syntax.Identifier)
variableToJavaIdentifier name =  
  let v = (Core.unName name)
  in (Logic.ifElse (Equality.equal v "_") (Syntax.Identifier "ignored") (Syntax.Identifier (sanitizeJavaName v)))

varDeclarationStatement :: (Syntax.Identifier -> Syntax.Expression -> Syntax.BlockStatement)
varDeclarationStatement id rhs = (Syntax.BlockStatementLocalVariableDeclaration (Syntax.LocalVariableDeclarationStatement (Syntax.LocalVariableDeclaration {
  Syntax.localVariableDeclarationModifiers = [],
  Syntax.localVariableDeclarationType = Syntax.LocalVariableTypeVar,
  Syntax.localVariableDeclarationDeclarators = [
    javaVariableDeclarator id (Just (Syntax.VariableInitializerExpression rhs))]})))

sanitizeJavaName :: (String -> String)
sanitizeJavaName name = (Logic.ifElse (isEscaped name) (unescape name) (Logic.ifElse (Equality.equal name "_") "ignored" (Formatting.sanitizeWithUnderscores Language.reservedWords name)))

isEscaped :: (String -> Bool)
isEscaped s = (Equality.equal (Strings.charAt 0 s) 36)

unescape :: (String -> String)
unescape s = (Strings.fromList (Lists.tail (Strings.toList s)))

javaPackageDeclaration :: (Module.Namespace -> Syntax.PackageDeclaration)
javaPackageDeclaration ns = Syntax.PackageDeclaration {
  Syntax.packageDeclarationModifiers = [],
  Syntax.packageDeclarationIdentifiers = (Lists.map (\s -> Syntax.Identifier s) (Strings.splitOn "." (Module.unNamespace ns)))}

overrideAnnotation :: Syntax.Annotation
overrideAnnotation = (Syntax.AnnotationMarker (Syntax.MarkerAnnotation (javaTypeName (Syntax.Identifier "Override"))))

methodInvocation :: (Maybe (Either Syntax.ExpressionName Syntax.Primary) -> Syntax.Identifier -> [Syntax.Expression] -> Syntax.MethodInvocation)
methodInvocation lhs methodName args =  
  let header = (Maybes.cases lhs (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName methodName)) (\either -> Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
          Syntax.methodInvocation_ComplexVariant = (Eithers.either (\en -> Syntax.MethodInvocation_VariantExpression en) (\p -> Syntax.MethodInvocation_VariantPrimary p) either),
          Syntax.methodInvocation_ComplexTypeArguments = [],
          Syntax.methodInvocation_ComplexIdentifier = methodName})))
  in Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = args}

methodInvocationStatic :: (Syntax.Identifier -> Syntax.Identifier -> [Syntax.Expression] -> Syntax.MethodInvocation)
methodInvocationStatic self methodName args = (methodInvocation (Just (Left (javaIdentifierToJavaExpressionName self))) methodName args)

methodDeclaration :: ([Syntax.MethodModifier] -> [Syntax.TypeParameter] -> [Syntax.Annotation] -> String -> [Syntax.FormalParameter] -> Syntax.Result -> Maybe [Syntax.BlockStatement] -> Syntax.ClassBodyDeclaration)
methodDeclaration mods tparams anns methodName params result stmts = (javaMethodDeclarationToJavaClassBodyDeclaration (Syntax.MethodDeclaration {
  Syntax.methodDeclarationAnnotations = anns,
  Syntax.methodDeclarationModifiers = mods,
  Syntax.methodDeclarationHeader = (javaMethodHeader tparams methodName params result),
  Syntax.methodDeclarationBody = (javaMethodBody stmts)}))

interfaceMethodDeclaration :: ([Syntax.InterfaceMethodModifier] -> [Syntax.TypeParameter] -> String -> [Syntax.FormalParameter] -> Syntax.Result -> Maybe [Syntax.BlockStatement] -> Syntax.InterfaceMemberDeclaration)
interfaceMethodDeclaration mods tparams methodName params result stmts = (Syntax.InterfaceMemberDeclarationInterfaceMethod (Syntax.InterfaceMethodDeclaration {
  Syntax.interfaceMethodDeclarationModifiers = mods,
  Syntax.interfaceMethodDeclarationHeader = (javaMethodHeader tparams methodName params result),
  Syntax.interfaceMethodDeclarationBody = (javaMethodBody stmts)}))

javaEquals :: (Syntax.EqualityExpression -> Syntax.RelationalExpression -> Syntax.EqualityExpression)
javaEquals lhs rhs = (Syntax.EqualityExpressionEqual (Syntax.EqualityExpression_Binary {
  Syntax.equalityExpression_BinaryLhs = lhs,
  Syntax.equalityExpression_BinaryRhs = rhs}))

javaEqualsNull :: (Syntax.EqualityExpression -> Syntax.EqualityExpression)
javaEqualsNull lhs = (javaEquals lhs (javaLiteralToJavaRelationalExpression Syntax.LiteralNull))

javaInstanceOf :: (Syntax.RelationalExpression -> Syntax.ReferenceType -> Syntax.RelationalExpression)
javaInstanceOf lhs rhs = (Syntax.RelationalExpressionInstanceof (Syntax.RelationalExpression_InstanceOf {
  Syntax.relationalExpression_InstanceOfLhs = lhs,
  Syntax.relationalExpression_InstanceOfRhs = rhs}))

javaThrowIllegalArgumentException :: ([Syntax.Expression] -> Syntax.Statement)
javaThrowIllegalArgumentException args = (javaThrowStatement (javaConstructorCall (javaConstructorName (Syntax.Identifier "IllegalArgumentException") Nothing) args Nothing))

javaThrowIllegalStateException :: ([Syntax.Expression] -> Syntax.Statement)
javaThrowIllegalStateException args = (javaThrowStatement (javaConstructorCall (javaConstructorName (Syntax.Identifier "IllegalStateException") Nothing) args Nothing))

addExpressions :: ([Syntax.MultiplicativeExpression] -> Syntax.AdditiveExpression)
addExpressions exprs =  
  let first = (Syntax.AdditiveExpressionUnary (Lists.head exprs)) 
      rest = (Lists.tail exprs)
  in (Lists.foldl (\ae -> \me -> Syntax.AdditiveExpressionPlus (Syntax.AdditiveExpression_Binary {
    Syntax.additiveExpression_BinaryLhs = ae,
    Syntax.additiveExpression_BinaryRhs = me})) first rest)

javaRelationalExpressionToJavaEqualityExpression :: (Syntax.RelationalExpression -> Syntax.EqualityExpression)
javaRelationalExpressionToJavaEqualityExpression re = (Syntax.EqualityExpressionUnary re)

nameToQualifiedJavaName :: (Helpers.Aliases -> Bool -> Core.Name -> Maybe String -> (Syntax.TypeIdentifier, Syntax.ClassTypeQualifier))
nameToQualifiedJavaName aliases qualify name mlocal =  
  let qn = (Names_.qualifyName name) 
      ns_ = (Module.qualifiedNameNamespace qn)
      local = (Module.qualifiedNameLocal qn)
      alias = (Maybes.cases ns_ Nothing (\n -> Just (Maybes.cases (Maps.lookup n (Helpers.aliasesPackages aliases)) (Names.javaPackageName (Strings.splitOn "." (Module.unNamespace n))) (\id -> id))))
      pkg = (Logic.ifElse qualify (Maybes.cases alias Syntax.ClassTypeQualifierNone (\p -> Syntax.ClassTypeQualifierPackage p)) Syntax.ClassTypeQualifierNone)
      jid = (javaTypeIdentifier (Maybes.cases mlocal (sanitizeJavaName local) (\l -> Strings.cat2 (Strings.cat2 (sanitizeJavaName local) ".") (sanitizeJavaName l))))
  in (jid, pkg)

nameToJavaClassType :: (Helpers.Aliases -> Bool -> [Syntax.TypeArgument] -> Core.Name -> Maybe String -> Syntax.ClassType)
nameToJavaClassType aliases qualify args name mlocal =  
  let result = (nameToQualifiedJavaName aliases qualify name mlocal) 
      id = (Pairs.first result)
      pkg = (Pairs.second result)
  in Syntax.ClassType {
    Syntax.classTypeAnnotations = [],
    Syntax.classTypeQualifier = pkg,
    Syntax.classTypeIdentifier = id,
    Syntax.classTypeArguments = args}

nameToJavaReferenceType :: (Helpers.Aliases -> Bool -> [Syntax.TypeArgument] -> Core.Name -> Maybe String -> Syntax.ReferenceType)
nameToJavaReferenceType aliases qualify args name mlocal = (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (nameToJavaClassType aliases qualify args name mlocal)))

nameToJavaName :: (Helpers.Aliases -> Core.Name -> Syntax.Identifier)
nameToJavaName aliases name =  
  let qn = (Names_.qualifyName name) 
      ns_ = (Module.qualifiedNameNamespace qn)
      local = (Module.qualifiedNameLocal qn)
  in (Logic.ifElse (isEscaped (Core.unName name)) (Syntax.Identifier (sanitizeJavaName local)) (Maybes.cases ns_ (Syntax.Identifier local) (\gname ->  
    let parts = (Maybes.cases (Maps.lookup gname (Helpers.aliasesPackages aliases)) (Strings.splitOn "." (Module.unNamespace gname)) (\pkgName -> Lists.map (\i -> Syntax.unIdentifier i) (Syntax.unPackageName pkgName))) 
        allParts = (Lists.concat2 parts [
                sanitizeJavaName local])
    in (Syntax.Identifier (Strings.intercalate "." allParts)))))

nameToJavaTypeIdentifier :: (Helpers.Aliases -> Bool -> Core.Name -> Syntax.TypeIdentifier)
nameToJavaTypeIdentifier aliases qualify name = (Pairs.first (nameToQualifiedJavaName aliases qualify name Nothing))

javaTypeFromTypeName :: (Helpers.Aliases -> Core.Name -> Syntax.Type)
javaTypeFromTypeName aliases elName = (javaTypeVariableToType (Syntax.TypeVariable {
  Syntax.typeVariableAnnotations = [],
  Syntax.typeVariableIdentifier = (nameToJavaTypeIdentifier aliases False elName)}))

javaDoubleCastExpression :: (Syntax.ReferenceType -> Syntax.ReferenceType -> Syntax.UnaryExpression -> Syntax.CastExpression)
javaDoubleCastExpression rawRt targetRt expr =  
  let firstCast = (javaCastExpressionToJavaExpression (javaCastExpression rawRt expr))
  in (javaCastExpression targetRt (javaExpressionToJavaUnaryExpression firstCast))

javaDoubleCastExpressionToJavaExpression :: (Syntax.ReferenceType -> Syntax.ReferenceType -> Syntax.UnaryExpression -> Syntax.Expression)
javaDoubleCastExpressionToJavaExpression rawRt targetRt expr = (javaCastExpressionToJavaExpression (javaDoubleCastExpression rawRt targetRt expr))

javaBytePrimitiveType :: Syntax.PrimitiveTypeWithAnnotations
javaBytePrimitiveType = Syntax.PrimitiveTypeWithAnnotations {
  Syntax.primitiveTypeWithAnnotationsType = (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)),
  Syntax.primitiveTypeWithAnnotationsAnnotations = []}

visitorTypeVariable :: Syntax.ReferenceType
visitorTypeVariable = (javaTypeVariable "r")

lookupJavaVarName :: (Helpers.Aliases -> Core.Name -> Core.Name)
lookupJavaVarName aliases name = (Maybes.cases (Maps.lookup name (Helpers.aliasesVarRenames aliases)) name (\renamed -> renamed))

variantClassName :: (Bool -> Core.Name -> Core.Name -> Core.Name)
variantClassName qualify elName fname =  
  let qn = (Names_.qualifyName elName) 
      ns_ = (Module.qualifiedNameNamespace qn)
      local = (Module.qualifiedNameLocal qn)
      flocal = (Formatting.capitalize (Core.unName fname))
      local1 = (Logic.ifElse qualify (Strings.cat2 (Strings.cat2 local ".") flocal) (Logic.ifElse (Equality.equal flocal local) (Strings.cat2 flocal "_") flocal))
  in (Names_.unqualifyName (Module.QualifiedName {
    Module.qualifiedNameNamespace = ns_,
    Module.qualifiedNameLocal = local1}))

variableDeclarationStatement :: (t0 -> Syntax.Type -> Syntax.Identifier -> Syntax.Expression -> Syntax.BlockStatement)
variableDeclarationStatement aliases jtype id rhs =  
  let init_ = (Syntax.VariableInitializerExpression rhs) 
      vdec = (javaVariableDeclarator id (Just init_))
  in (Syntax.BlockStatementLocalVariableDeclaration (Syntax.LocalVariableDeclarationStatement (Syntax.LocalVariableDeclaration {
    Syntax.localVariableDeclarationModifiers = [],
    Syntax.localVariableDeclarationType = (Syntax.LocalVariableTypeType (Syntax.UnannType jtype)),
    Syntax.localVariableDeclarationDeclarators = [
      vdec]})))

javaStringMultiplicativeExpression :: (String -> Syntax.MultiplicativeExpression)
javaStringMultiplicativeExpression s = (javaLiteralToJavaMultiplicativeExpression (javaString s))

suppressWarningsUncheckedAnnotation :: Syntax.Annotation
suppressWarningsUncheckedAnnotation = (Syntax.AnnotationSingleElement (Syntax.SingleElementAnnotation {
  Syntax.singleElementAnnotationName = (javaTypeName (Syntax.Identifier "SuppressWarnings")),
  Syntax.singleElementAnnotationValue = (Just (Syntax.ElementValueConditionalExpression (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
    Syntax.ConditionalAndExpression [
      javaPostfixExpressionToJavaInclusiveOrExpression (Syntax.PostfixExpressionPrimary (javaLiteralToJavaPrimary (javaString "unchecked")))]]))))}))

methodInvocationStaticWithTypeArgs :: (Syntax.Identifier -> Syntax.Identifier -> [Syntax.TypeArgument] -> [Syntax.Expression] -> Syntax.MethodInvocation)
methodInvocationStaticWithTypeArgs self methodName targs args =  
  let header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
          Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (javaIdentifierToJavaExpressionName self)),
          Syntax.methodInvocation_ComplexTypeArguments = targs,
          Syntax.methodInvocation_ComplexIdentifier = methodName}))
  in Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = args}

javaArrayCreation :: (Syntax.PrimitiveTypeWithAnnotations -> Maybe Syntax.ArrayInitializer -> Syntax.Expression)
javaArrayCreation primType minit =  
  let init_ = (Maybes.cases minit (Syntax.ArrayInitializer []) (\i -> i))
  in (javaPrimaryToJavaExpression (Syntax.PrimaryArrayCreation (Syntax.ArrayCreationExpressionPrimitiveArray (Syntax.ArrayCreationExpression_PrimitiveArray {
    Syntax.arrayCreationExpression_PrimitiveArrayType = primType,
    Syntax.arrayCreationExpression_PrimitiveArrayDims = [],
    Syntax.arrayCreationExpression_PrimitiveArrayArray = init_}))))

javaArrayInitializer :: ([Syntax.Expression] -> Syntax.ArrayInitializer)
javaArrayInitializer exprs = (Syntax.ArrayInitializer [
  Lists.map (\e -> Syntax.VariableInitializerExpression e) exprs])

toAssignStmt :: (Core.Name -> Syntax.Statement)
toAssignStmt fname =  
  let id = (fieldNameToJavaIdentifier fname) 
      lhs = (Syntax.LeftHandSideFieldAccess (Syntax.FieldAccess {
              Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Syntax.PrimaryNoNewArray_ Syntax.PrimaryNoNewArrayThis)),
              Syntax.fieldAccessIdentifier = id}))
      rhs = (fieldNameToJavaExpression fname)
  in (javaAssignmentStatement lhs rhs)

unTypeParameter :: (Syntax.TypeParameter -> String)
unTypeParameter tp = (Syntax.unIdentifier (Syntax.unTypeIdentifier (Syntax.typeParameterIdentifier tp)))

importAliasesForModule :: (Module.Module -> Helpers.Aliases)
importAliasesForModule mod = Helpers.Aliases {
  Helpers.aliasesCurrentNamespace = (Module.moduleNamespace mod),
  Helpers.aliasesPackages = Maps.empty,
  Helpers.aliasesBranchVars = Sets.empty,
  Helpers.aliasesRecursiveVars = Sets.empty,
  Helpers.aliasesInScopeTypeParams = Sets.empty,
  Helpers.aliasesPolymorphicLocals = Sets.empty,
  Helpers.aliasesInScopeJavaVars = Sets.empty,
  Helpers.aliasesVarRenames = Maps.empty,
  Helpers.aliasesLambdaVars = Sets.empty,
  Helpers.aliasesTypeVarSubst = Maps.empty,
  Helpers.aliasesTrustedTypeVars = Sets.empty,
  Helpers.aliasesMethodCodomain = Nothing,
  Helpers.aliasesThunkedVars = Sets.empty}

javaClassDeclaration :: (Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Syntax.ClassModifier] -> Maybe Core.Name -> [Syntax.InterfaceType] -> [Syntax.ClassBodyDeclarationWithComments] -> Syntax.ClassDeclaration)
javaClassDeclaration aliases tparams elName mods supname impls bodyDecls =  
  let extends_ = (Maybes.map (\n -> nameToJavaClassType aliases True [] n Nothing) supname)
  in (Syntax.ClassDeclarationNormal (Syntax.NormalClassDeclaration {
    Syntax.normalClassDeclarationModifiers = mods,
    Syntax.normalClassDeclarationIdentifier = (javaDeclName elName),
    Syntax.normalClassDeclarationParameters = tparams,
    Syntax.normalClassDeclarationExtends = extends_,
    Syntax.normalClassDeclarationImplements = impls,
    Syntax.normalClassDeclarationBody = (Syntax.ClassBody bodyDecls)}))

makeConstructor :: (Helpers.Aliases -> Core.Name -> Bool -> [Syntax.FormalParameter] -> [Syntax.BlockStatement] -> Syntax.ClassBodyDeclaration)
makeConstructor aliases elName private params stmts =  
  let nm = (Syntax.SimpleTypeName (nameToJavaTypeIdentifier aliases False elName)) 
      cons = Syntax.ConstructorDeclarator {
              Syntax.constructorDeclaratorParameters = [],
              Syntax.constructorDeclaratorName = nm,
              Syntax.constructorDeclaratorReceiverParameter = Nothing,
              Syntax.constructorDeclaratorFormalParameters = params}
      mods = [
              Logic.ifElse private Syntax.ConstructorModifierPrivate Syntax.ConstructorModifierPublic]
      body = Syntax.ConstructorBody {
              Syntax.constructorBodyInvocation = Nothing,
              Syntax.constructorBodyStatements = stmts}
  in (Syntax.ClassBodyDeclarationConstructorDeclaration (Syntax.ConstructorDeclaration {
    Syntax.constructorDeclarationModifiers = mods,
    Syntax.constructorDeclarationConstructor = cons,
    Syntax.constructorDeclarationThrows = Nothing,
    Syntax.constructorDeclarationBody = body}))

toAcceptMethod :: (Bool -> [Syntax.TypeParameter] -> Syntax.ClassBodyDeclaration)
toAcceptMethod abstract vtparams =  
  let mods = (Logic.ifElse abstract [
          Syntax.MethodModifierPublic,
          Syntax.MethodModifierAbstract] [
          Syntax.MethodModifierPublic]) 
      tparams = [
              javaTypeParameter Names.visitorReturnParameter]
      anns = (Logic.ifElse abstract [] [
              overrideAnnotation])
      typeArgs = (Lists.map (\tp -> Syntax.TypeArgumentReference (typeParameterToReferenceType tp)) vtparams)
      ref = (javaClassTypeToJavaType (Syntax.ClassType {
              Syntax.classTypeAnnotations = [],
              Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
              Syntax.classTypeIdentifier = (javaTypeIdentifier Names.visitorName),
              Syntax.classTypeArguments = (Lists.concat2 typeArgs [
                Syntax.TypeArgumentReference visitorTypeVariable])}))
      param = (javaTypeToJavaFormalParameter ref (Core.Name "visitor"))
      result = (javaTypeToJavaResult (Syntax.TypeReference visitorTypeVariable))
      returnExpr = (javaMethodInvocationToJavaExpression (methodInvocationStatic (Syntax.Identifier "visitor") (Syntax.Identifier Names.visitMethodName) [
              javaThis]))
      body = (Logic.ifElse abstract Nothing (Just [
              Syntax.BlockStatementStatement (javaReturnStatement (Just returnExpr))]))
  in (methodDeclaration mods tparams anns Names.acceptMethodName [
    param] result body)

toJavaArrayType :: (Syntax.Type -> Compute.Flow t0 Syntax.Type)
toJavaArrayType t = ((\x -> case x of
  Syntax.TypeReference v1 -> ((\x -> case x of
    Syntax.ReferenceTypeClassOrInterface v2 -> (Flows.pure (Syntax.TypeReference (Syntax.ReferenceTypeArray (Syntax.ArrayType {
      Syntax.arrayTypeDims = (Syntax.Dims [
        []]),
      Syntax.arrayTypeVariant = (Syntax.ArrayType_VariantClassOrInterface v2)}))))
    Syntax.ReferenceTypeArray v2 ->  
      let oldDims = (Syntax.unDims (Syntax.arrayTypeDims v2)) 
          newDims = (Syntax.Dims (Lists.concat2 oldDims [
                  []]))
          variant = (Syntax.arrayTypeVariant v2)
      in (Flows.pure (Syntax.TypeReference (Syntax.ReferenceTypeArray (Syntax.ArrayType {
        Syntax.arrayTypeDims = newDims,
        Syntax.arrayTypeVariant = variant}))))
    Syntax.ReferenceTypeVariable _ -> (Flows.fail "don't know how to make Java reference type into array type")) v1)
  Syntax.TypePrimitive _ -> (Flows.fail "don't know how to make Java type into array type")) t)

javaTypeToJavaReferenceType :: (Syntax.Type -> Compute.Flow t0 Syntax.ReferenceType)
javaTypeToJavaReferenceType t = ((\x -> case x of
  Syntax.TypeReference v1 -> (Flows.pure v1)
  Syntax.TypePrimitive _ -> (Flows.fail "expected a Java reference type")) t)

javaReferenceTypeToRawType :: (Syntax.ReferenceType -> Syntax.ReferenceType)
javaReferenceTypeToRawType rt = ((\x -> case x of
  Syntax.ReferenceTypeClassOrInterface v1 -> ((\x -> case x of
    Syntax.ClassOrInterfaceTypeClass v2 ->  
      let anns = (Syntax.classTypeAnnotations v2) 
          qual = (Syntax.classTypeQualifier v2)
          id = (Syntax.classTypeIdentifier v2)
      in (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Syntax.ClassType {
        Syntax.classTypeAnnotations = anns,
        Syntax.classTypeQualifier = qual,
        Syntax.classTypeIdentifier = id,
        Syntax.classTypeArguments = []})))
    Syntax.ClassOrInterfaceTypeInterface v2 ->  
      let ct = (Syntax.unInterfaceType v2) 
          anns = (Syntax.classTypeAnnotations ct)
          qual = (Syntax.classTypeQualifier ct)
          id = (Syntax.classTypeIdentifier ct)
      in (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeInterface (Syntax.InterfaceType (Syntax.ClassType {
        Syntax.classTypeAnnotations = anns,
        Syntax.classTypeQualifier = qual,
        Syntax.classTypeIdentifier = id,
        Syntax.classTypeArguments = []}))))) v1)
  _ -> rt) rt)

addJavaTypeParameter :: (Syntax.ReferenceType -> Syntax.Type -> Compute.Flow t0 Syntax.Type)
addJavaTypeParameter rt t = ((\x -> case x of
  Syntax.TypeReference v1 -> ((\x -> case x of
    Syntax.ReferenceTypeClassOrInterface v2 -> ((\x -> case x of
      Syntax.ClassOrInterfaceTypeClass v3 ->  
        let anns = (Syntax.classTypeAnnotations v3) 
            qual = (Syntax.classTypeQualifier v3)
            id = (Syntax.classTypeIdentifier v3)
            args = (Syntax.classTypeArguments v3)
        in (Flows.pure (Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Syntax.ClassType {
          Syntax.classTypeAnnotations = anns,
          Syntax.classTypeQualifier = qual,
          Syntax.classTypeIdentifier = id,
          Syntax.classTypeArguments = (Lists.concat2 args [
            Syntax.TypeArgumentReference rt])})))))
      Syntax.ClassOrInterfaceTypeInterface _ -> (Flows.fail "expected a Java class type")) v2)
    Syntax.ReferenceTypeVariable v2 -> (Flows.pure (javaTypeVariableToType v2))
    Syntax.ReferenceTypeArray _ -> (Flows.fail "expected a Java class or interface type, or a variable")) v1)
  Syntax.TypePrimitive _ -> (Flows.fail "expected a reference type")) t)

uniqueVarName :: (Helpers.Aliases -> Core.Name -> Core.Name)
uniqueVarName aliases name = (Logic.ifElse (Sets.member name (Helpers.aliasesInScopeJavaVars aliases)) (uniqueVarName_go aliases (Core.unName name) 2) name)

uniqueVarName_go :: (Helpers.Aliases -> String -> Int -> Core.Name)
uniqueVarName_go aliases base n =  
  let candidate = (Core.Name (Strings.cat2 base (Literals.showInt32 n)))
  in (Logic.ifElse (Sets.member candidate (Helpers.aliasesInScopeJavaVars aliases)) (uniqueVarName_go aliases base (Math.add n 1)) candidate)

addInScopeVar :: (Core.Name -> Helpers.Aliases -> Helpers.Aliases)
addInScopeVar name aliases = Helpers.Aliases {
  Helpers.aliasesCurrentNamespace = (Helpers.aliasesCurrentNamespace aliases),
  Helpers.aliasesPackages = (Helpers.aliasesPackages aliases),
  Helpers.aliasesBranchVars = (Helpers.aliasesBranchVars aliases),
  Helpers.aliasesRecursiveVars = (Helpers.aliasesRecursiveVars aliases),
  Helpers.aliasesInScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases),
  Helpers.aliasesPolymorphicLocals = (Helpers.aliasesPolymorphicLocals aliases),
  Helpers.aliasesInScopeJavaVars = (Sets.insert name (Helpers.aliasesInScopeJavaVars aliases)),
  Helpers.aliasesVarRenames = (Helpers.aliasesVarRenames aliases),
  Helpers.aliasesLambdaVars = (Helpers.aliasesLambdaVars aliases),
  Helpers.aliasesTypeVarSubst = (Helpers.aliasesTypeVarSubst aliases),
  Helpers.aliasesTrustedTypeVars = (Helpers.aliasesTrustedTypeVars aliases),
  Helpers.aliasesMethodCodomain = (Helpers.aliasesMethodCodomain aliases),
  Helpers.aliasesThunkedVars = (Helpers.aliasesThunkedVars aliases)}

addInScopeVars :: ([Core.Name] -> Helpers.Aliases -> Helpers.Aliases)
addInScopeVars names aliases = (Lists.foldl (\a -> \n -> addInScopeVar n a) aliases names)

addVarRename :: (Core.Name -> Core.Name -> Helpers.Aliases -> Helpers.Aliases)
addVarRename original renamed aliases = Helpers.Aliases {
  Helpers.aliasesCurrentNamespace = (Helpers.aliasesCurrentNamespace aliases),
  Helpers.aliasesPackages = (Helpers.aliasesPackages aliases),
  Helpers.aliasesBranchVars = (Helpers.aliasesBranchVars aliases),
  Helpers.aliasesRecursiveVars = (Helpers.aliasesRecursiveVars aliases),
  Helpers.aliasesInScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases),
  Helpers.aliasesPolymorphicLocals = (Helpers.aliasesPolymorphicLocals aliases),
  Helpers.aliasesInScopeJavaVars = (Helpers.aliasesInScopeJavaVars aliases),
  Helpers.aliasesVarRenames = (Maps.insert original renamed (Helpers.aliasesVarRenames aliases)),
  Helpers.aliasesLambdaVars = (Helpers.aliasesLambdaVars aliases),
  Helpers.aliasesTypeVarSubst = (Helpers.aliasesTypeVarSubst aliases),
  Helpers.aliasesTrustedTypeVars = (Helpers.aliasesTrustedTypeVars aliases),
  Helpers.aliasesMethodCodomain = (Helpers.aliasesMethodCodomain aliases),
  Helpers.aliasesThunkedVars = (Helpers.aliasesThunkedVars aliases)}
