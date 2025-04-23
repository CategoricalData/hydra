-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Cpp.Utils where

import Hydra.Staging.Formatting
import qualified Hydra.Ext.Cpp.Syntax as Cpp

import qualified Data.List as L


cppPostfixExpressionToCppExpression :: Cpp.PostfixExpression -> Cpp.Expression
cppPostfixExpressionToCppExpression pfe =
  Cpp.ExpressionAssignment $
    Cpp.AssignmentExpressionConditional $
      Cpp.ConditionalExpressionLogicalOr $
        Cpp.LogicalOrExpressionLogicalAnd $
          Cpp.LogicalAndExpressionInclusiveOr $
            Cpp.InclusiveOrExpressionExclusiveOr $
              Cpp.ExclusiveOrExpressionAnd $
                Cpp.AndExpressionEquality $
                  Cpp.EqualityExpressionRelational $
                    Cpp.RelationalExpressionShift $
                      Cpp.ShiftExpressionAdditive $
                        Cpp.AdditiveExpressionMultiplicative $
                          Cpp.MultiplicativeExpressionUnary $
                            Cpp.UnaryExpressionPostfix pfe

cppPrimaryExpressionToCppExpression :: Cpp.PrimaryExpression -> Cpp.Expression
cppPrimaryExpressionToCppExpression prim = cppPostfixExpressionToCppExpression $ Cpp.PostfixExpressionPrimary prim

createBasicType :: String -> Cpp.TypeExpression
createBasicType name = Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed name

createCompoundStmt :: [Cpp.Statement] -> Cpp.CompoundStatement
createCompoundStmt = Cpp.CompoundStatement

createConstRefType :: Cpp.TypeExpression -> Cpp.TypeExpression
createConstRefType baseType = createReferenceType $ createConstType baseType

createConstType :: Cpp.TypeExpression -> Cpp.TypeExpression
createConstType baseType = createQualifiedType baseType Cpp.TypeQualifierConst

createFunctionCallExpr :: String -> [Cpp.Expression] -> Cpp.Expression
createFunctionCallExpr funcName args =
  cppPostfixExpressionToCppExpression $
    Cpp.PostfixExpressionFunctionCall $
      Cpp.FunctionCallOperation
        (Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier funcName)
        args

createIdentifierExpr :: String -> Cpp.Expression
createIdentifierExpr name = cppPrimaryExpressionToCppExpression $ Cpp.PrimaryExpressionIdentifier name

--createLambdaExpr :: String -> Cpp.Expression
createLambdaExpr =
  cppPrimaryExpressionToCppExpression $
    Cpp.PrimaryExpressionLambda $
      Cpp.LambdaExpression
        (Cpp.CaptureListCaptureByValue)  -- Empty capture list
        [Cpp.Parameter
          (createConstRefType $ Cpp.TypeExpressionBasic $ Cpp.BasicTypeAuto)
          "arg"
          Nothing]
        Nothing  -- No explicit return type
        (Cpp.CompoundStatement [
          Cpp.StatementJump $ Cpp.JumpStatementReturnValue $
            createFunctionCallExpr "typeid" [createIdentifierExpr "arg"]
        ])

createLiteralBoolExpr :: Bool -> Cpp.Expression
createLiteralBoolExpr val = cppPrimaryExpressionToCppExpression $ Cpp.PrimaryExpressionLiteral $
  Cpp.LiteralBoolean $ Cpp.BooleanLiteral val

createLiteralIntExpr :: Int -> Cpp.Expression
createLiteralIntExpr val = cppPrimaryExpressionToCppExpression $ Cpp.PrimaryExpressionLiteral $
  Cpp.LiteralInteger $ Cpp.IntegerLiteralDecimal $ fromIntegral val

createLiteralStringExpr :: String -> Cpp.Expression
createLiteralStringExpr val = cppPrimaryExpressionToCppExpression $ Cpp.PrimaryExpressionLiteral $
  Cpp.LiteralString $ Cpp.StringLiteral val

createMemberAccessExpr :: Cpp.Expression -> String -> Cpp.Expression
createMemberAccessExpr objExpr member =
  cppPostfixExpressionToCppExpression $
    Cpp.PostfixExpressionMemberAccess $
      Cpp.MemberAccessOperation
        (extractPostfixExpression objExpr)
        member

createParameter :: Cpp.TypeExpression -> String -> Maybe Cpp.Expression -> Cpp.Parameter
createParameter typ name defaultVal = Cpp.Parameter typ name defaultVal

createQualifiedType :: Cpp.TypeExpression -> Cpp.TypeQualifier -> Cpp.TypeExpression
createQualifiedType baseType qualifier =
  Cpp.TypeExpressionQualified $ Cpp.QualifiedType baseType qualifier

createReferenceType :: Cpp.TypeExpression -> Cpp.TypeExpression
createReferenceType baseType = createQualifiedType baseType Cpp.TypeQualifierLvalueRef

createReturnStmt :: Cpp.Expression -> Cpp.Statement
createReturnStmt expr = Cpp.StatementJump $ Cpp.JumpStatementReturnValue expr

createReturnVoidStmt :: Cpp.Statement
createReturnVoidStmt = Cpp.StatementJump $ Cpp.JumpStatementReturnVoid

createTemplateType :: String -> [Cpp.TypeExpression] -> Cpp.TypeExpression
createTemplateType name args =
  Cpp.TypeExpressionTemplate $
    Cpp.TemplateType name [Cpp.TemplateArgumentType a | a <- args]

extractPostfixExpression :: Cpp.Expression -> Cpp.PostfixExpression
extractPostfixExpression (Cpp.ExpressionAssignment
  (Cpp.AssignmentExpressionConditional
    (Cpp.ConditionalExpressionLogicalOr
      (Cpp.LogicalOrExpressionLogicalAnd
        (Cpp.LogicalAndExpressionInclusiveOr
          (Cpp.InclusiveOrExpressionExclusiveOr
            (Cpp.ExclusiveOrExpressionAnd
              (Cpp.AndExpressionEquality
                (Cpp.EqualityExpressionRelational
                  (Cpp.RelationalExpressionShift
                    (Cpp.ShiftExpressionAdditive
                      (Cpp.AdditiveExpressionMultiplicative
                        (Cpp.MultiplicativeExpressionUnary
                          (Cpp.UnaryExpressionPostfix postfix)))))))))))))) = postfix
extractPostfixExpression _ = Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier "error"

normalizeComment :: String -> String
normalizeComment s = if L.null stripped
    then ""
    else if L.last stripped /= '.'
      then stripped ++ "."
      else stripped
  where
    stripped = stripLeadingAndTrailingWhitespace s
