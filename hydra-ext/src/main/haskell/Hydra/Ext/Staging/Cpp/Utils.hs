-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Staging.Cpp.Utils where

import Hydra.Kernel
import qualified Hydra.Ext.Cpp.Syntax as Cpp

import qualified Data.List as L
import qualified Data.Map as M


-- | Environment for C++ code generation
data CppEnvironment = CppEnvironment {
  cppEnvironmentNamespaces :: Namespaces String,
  cppEnvironmentBoundTypeVariables :: ([Name], M.Map Name String)}

constParameter :: String -> Cpp.TypeExpression -> Cpp.Parameter
constParameter name typ = Cpp.Parameter
  (Cpp.TypeExpressionQualified $
    Cpp.QualifiedType
      (Cpp.TypeExpressionQualified $
        Cpp.QualifiedType typ Cpp.TypeQualifierConst)
      Cpp.TypeQualifierLvalueRef)
  name
  False
  Nothing

cppClassDeclaration :: String -> [Cpp.BaseSpecifier] -> Maybe Cpp.ClassBody -> Cpp.Declaration
cppClassDeclaration name baseSpecs mbody = Cpp.DeclarationClass $ Cpp.ClassDeclaration
  (Cpp.ClassSpecifier Cpp.ClassKeyClass name baseSpecs)
  mbody

cppEnumDeclaration :: String -> Maybe Cpp.ClassBody -> Cpp.Declaration
cppEnumDeclaration name mbody = Cpp.DeclarationClass $ Cpp.ClassDeclaration
  -- Note: "enum class" instead of "enum" to avoid collisions on enum values
  (Cpp.ClassSpecifier Cpp.ClassKeyEnumClass name [])
  mbody

cppEnumForwardDeclaration :: String -> Cpp.Declaration
cppEnumForwardDeclaration name = Cpp.DeclarationClass $ Cpp.ClassDeclaration
  (Cpp.ClassSpecifier Cpp.ClassKeyEnumClass name []) Nothing

cppPostfixExpressionToCppExpression :: Cpp.PostfixExpression -> Cpp.Expression
cppPostfixExpressionToCppExpression = cppUnaryExpressionToCppExpression . Cpp.UnaryExpressionPostfix

cppPrimaryExpressionToCppExpression :: Cpp.PrimaryExpression -> Cpp.Expression
cppPrimaryExpressionToCppExpression prim = cppPostfixExpressionToCppExpression $ Cpp.PostfixExpressionPrimary prim

cppUnaryExpressionToCppExpression :: Cpp.UnaryExpression -> Cpp.Expression
cppUnaryExpressionToCppExpression ue =
  Cpp.ExpressionAssignment $
    Cpp.AssignmentExpressionConditional $
      Cpp.ConditionalExpressionLogicalOr $ cppUnaryExpressionToCppLogicalOrExpression ue

cppUnaryExpressionToCppLogicalOrExpression :: Cpp.UnaryExpression -> Cpp.LogicalOrExpression
cppUnaryExpressionToCppLogicalOrExpression ue =
  Cpp.LogicalOrExpressionLogicalAnd $
    Cpp.LogicalAndExpressionInclusiveOr $
      Cpp.InclusiveOrExpressionExclusiveOr $
        Cpp.ExclusiveOrExpressionAnd $
          Cpp.AndExpressionEquality $
            Cpp.EqualityExpressionRelational $
              Cpp.RelationalExpressionShift $
                Cpp.ShiftExpressionAdditive $
                  Cpp.AdditiveExpressionMultiplicative $
                    Cpp.MultiplicativeExpressionUnary ue

createCastExpr :: Cpp.TypeExpression -> Cpp.Expression -> Cpp.Expression
createCastExpr targetType expr = cppPrimaryExpressionToCppExpression $
  Cpp.PrimaryExpressionParenthesized $
    cppPrimaryExpressionToCppExpression $ Cpp.PrimaryExpressionParenthesized expr

createCompoundStmt :: [Cpp.Statement] -> Cpp.CompoundStatement
createCompoundStmt = Cpp.CompoundStatement

createConstRefType :: Cpp.TypeExpression -> Cpp.TypeExpression
createConstRefType baseType = createReferenceType $ createConstType baseType

createConstType :: Cpp.TypeExpression -> Cpp.TypeExpression
createConstType baseType = createQualifiedType baseType Cpp.TypeQualifierConst

createConstructorBody :: [Cpp.Parameter] -> Cpp.FunctionBody
createConstructorBody params = if L.null params then Cpp.FunctionBodyDefault else emptyFunctionBody

createEnumAccessExpr :: String -> String -> Cpp.Expression
createEnumAccessExpr enumName valueName = cppPostfixExpressionToCppExpression $
  Cpp.PostfixExpressionMemberAccess $ Cpp.MemberAccessOperation
    (Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier enumName)
    valueName

createFunctionCallExpr :: String -> [Cpp.Expression] -> Cpp.Expression
createFunctionCallExpr funcName args =
  cppPostfixExpressionToCppExpression $
    Cpp.PostfixExpressionFunctionCall $
      Cpp.FunctionCallOperation
        (Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier funcName)
        args

createHeaderFile :: [Cpp.IncludeDirective] -> [Cpp.Declaration] -> Cpp.Program
createHeaderFile includes decls = Cpp.Program
  [Cpp.PreprocessorDirectivePragma $ Cpp.PragmaDirective "once"]
  includes
  decls

createIdentifierExpr :: String -> Cpp.Expression
createIdentifierExpr name = cppPrimaryExpressionToCppExpression $ Cpp.PrimaryExpressionIdentifier name

createLambdaExpr :: [Cpp.Parameter] -> Cpp.Expression -> Cpp.Expression
createLambdaExpr params body = cppPostfixExpressionToCppExpression $
  Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionLambda $
    Cpp.LambdaExpression
      (Cpp.CaptureListCaptures [])
      params
      Nothing
      (Cpp.CompoundStatement [Cpp.StatementExpression body])

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
  Cpp.TypeExpressionTemplate $ Cpp.TemplateType name [Cpp.TemplateArgumentType a | a <- args]

createThisExpr :: Cpp.Expression
createThisExpr = cppPostfixExpressionToCppExpression $
  Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier "*this"

createThrowStmt :: String -> Cpp.Expression -> Cpp.Statement
createThrowStmt exceptionType arg =
  Cpp.StatementJump $ Cpp.JumpStatementThrow $
    createFunctionCallExpr exceptionType [arg]

createTypeIdNameCall :: Cpp.Expression
createTypeIdNameCall =
  cppPostfixExpressionToCppExpression $
    Cpp.PostfixExpressionFunctionCall $
      Cpp.FunctionCallOperation
        (Cpp.PostfixExpressionMemberAccess $
          Cpp.MemberAccessOperation
            (Cpp.PostfixExpressionFunctionCall $
              Cpp.FunctionCallOperation
                (Cpp.PostfixExpressionPrimary $
                  Cpp.PrimaryExpressionIdentifier "typeid")
                [cppPrimaryExpressionToCppExpression $
                  Cpp.PrimaryExpressionParenthesized createThisExpr])
            "name")
        []

createTypeNameExpr :: String -> Cpp.Expression
createTypeNameExpr typeName = cppPostfixExpressionToCppExpression $
  Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier typeName

createVariantExpr :: Cpp.Expression
createVariantExpr = cppPostfixExpressionToCppExpression $
  Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier "variant"

emptyFunctionBody :: Cpp.FunctionBody
emptyFunctionBody = Cpp.FunctionBodyCompound $ Cpp.CompoundStatement []

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

memberSpecificationProtected :: Cpp.MemberSpecification
memberSpecificationProtected = Cpp.MemberSpecificationAccessLabel Cpp.AccessSpecifierProtected

memberSpecificationPublic :: Cpp.MemberSpecification
memberSpecificationPublic = Cpp.MemberSpecificationAccessLabel Cpp.AccessSpecifierPublic

normalizeComment :: String -> String
normalizeComment s = if L.null stripped
    then ""
    else if L.last stripped /= '.'
      then stripped ++ "."
      else stripped
  where
    stripped = stripLeadingAndTrailingWhitespace s

stringExpression :: String -> Cpp.Expression
stringExpression =
  cppPrimaryExpressionToCppExpression . Cpp.PrimaryExpressionLiteral . Cpp.LiteralString . Cpp.StringLiteral

toConstType :: Cpp.TypeExpression -> Cpp.TypeExpression
toConstType baseType = Cpp.TypeExpressionQualified $ Cpp.QualifiedType baseType Cpp.TypeQualifierConst

unnamedParameter :: String -> Cpp.TypeExpression -> Cpp.Parameter
unnamedParameter name typ = Cpp.Parameter typ name True Nothing
