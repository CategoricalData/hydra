-- Note: this is an automatically generated file. Do not edit.

-- | C++ utilities for constructing C++ syntax trees

module Hydra.Cpp.Utils where

import qualified Hydra.Cpp.Syntax as Syntax
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Create a const reference parameter
constParameter :: String -> Syntax.TypeExpression -> Syntax.Parameter
constParameter name typ =
    Syntax.Parameter {
      Syntax.parameterType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
        Syntax.qualifiedTypeBaseType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
          Syntax.qualifiedTypeBaseType = typ,
          Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierConst})),
        Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierLvalueRef})),
      Syntax.parameterName = name,
      Syntax.parameterUnnamed = False,
      Syntax.parameterDefaultValue = Nothing}

-- | Create a C++ class declaration
cppClassDeclaration :: String -> [Syntax.BaseSpecifier] -> Maybe Syntax.ClassBody -> Syntax.Declaration
cppClassDeclaration name baseSpecs mbody =
    Syntax.DeclarationClass (Syntax.ClassDeclaration {
      Syntax.classDeclarationSpecifier = Syntax.ClassSpecifier {
        Syntax.classSpecifierKey = Syntax.ClassKeyClass,
        Syntax.classSpecifierName = name,
        Syntax.classSpecifierInheritance = baseSpecs},
      Syntax.classDeclarationBody = mbody})

-- | Create a C++ enum class declaration
cppEnumDeclaration :: String -> Maybe Syntax.ClassBody -> Syntax.Declaration
cppEnumDeclaration name mbody =
    Syntax.DeclarationClass (Syntax.ClassDeclaration {
      Syntax.classDeclarationSpecifier = Syntax.ClassSpecifier {
        Syntax.classSpecifierKey = Syntax.ClassKeyEnumClass,
        Syntax.classSpecifierName = name,
        Syntax.classSpecifierInheritance = []},
      Syntax.classDeclarationBody = mbody})

-- | Create a C++ enum class forward declaration
cppEnumForwardDeclaration :: String -> Syntax.Declaration
cppEnumForwardDeclaration name =
    Syntax.DeclarationClass (Syntax.ClassDeclaration {
      Syntax.classDeclarationSpecifier = Syntax.ClassSpecifier {
        Syntax.classSpecifierKey = Syntax.ClassKeyEnumClass,
        Syntax.classSpecifierName = name,
        Syntax.classSpecifierInheritance = []},
      Syntax.classDeclarationBody = Nothing})

-- | Convert a PostfixExpression to an Expression
cppPostfixExpressionToCppExpression :: Syntax.PostfixExpression -> Syntax.Expression
cppPostfixExpressionToCppExpression pf = cppUnaryExpressionToCppExpression (Syntax.UnaryExpressionPostfix pf)

-- | Convert a PrimaryExpression to an Expression
cppPrimaryExpressionToCppExpression :: Syntax.PrimaryExpression -> Syntax.Expression
cppPrimaryExpressionToCppExpression prim = cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionPrimary prim)

-- | Convert a UnaryExpression to an Expression
cppUnaryExpressionToCppExpression :: Syntax.UnaryExpression -> Syntax.Expression
cppUnaryExpressionToCppExpression ue =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionLogicalOr (cppUnaryExpressionToCppLogicalOrExpression ue)))

-- | Convert a UnaryExpression to a LogicalOrExpression
cppUnaryExpressionToCppLogicalOrExpression :: Syntax.UnaryExpression -> Syntax.LogicalOrExpression
cppUnaryExpressionToCppLogicalOrExpression ue =
    Syntax.LogicalOrExpressionLogicalAnd (Syntax.LogicalAndExpressionInclusiveOr (Syntax.InclusiveOrExpressionExclusiveOr (Syntax.ExclusiveOrExpressionAnd (Syntax.AndExpressionEquality (Syntax.EqualityExpressionRelational (Syntax.RelationalExpressionShift (Syntax.ShiftExpressionAdditive (Syntax.AdditiveExpressionMultiplicative (Syntax.MultiplicativeExpressionUnary ue)))))))))

-- | Create a cast expression
createCastExpr :: t0 -> Syntax.Expression -> Syntax.Expression
createCastExpr targetType expr =
    cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionParenthesized (cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionParenthesized expr)))

-- | Create a compound statement
createCompoundStmt :: [Syntax.Statement] -> Syntax.CompoundStatement
createCompoundStmt stmts = Syntax.CompoundStatement stmts

-- | Create a const reference type
createConstRefType :: Syntax.TypeExpression -> Syntax.TypeExpression
createConstRefType baseType = createReferenceType (createConstType baseType)

-- | Create a const-qualified type
createConstType :: Syntax.TypeExpression -> Syntax.TypeExpression
createConstType baseType = createQualifiedType baseType Syntax.TypeQualifierConst

-- | Create a constructor body (default if no params, empty otherwise)
createConstructorBody :: [t0] -> Syntax.FunctionBody
createConstructorBody params = Logic.ifElse (Lists.null params) Syntax.FunctionBodyDefault emptyFunctionBody

-- | Create an enum access expression
createEnumAccessExpr :: String -> String -> Syntax.Expression
createEnumAccessExpr enumName valueName =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionMemberAccess (Syntax.MemberAccessOperation {
      Syntax.memberAccessOperationObject = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier enumName)),
      Syntax.memberAccessOperationMember = valueName}))

-- | Create a function call expression
createFunctionCallExpr :: String -> [Syntax.Expression] -> Syntax.Expression
createFunctionCallExpr funcName args =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
      Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier funcName)),
      Syntax.functionCallOperationArguments = args}))

-- | Create a header file with pragma once
createHeaderFile :: [Syntax.IncludeDirective] -> [Syntax.Declaration] -> Syntax.Program
createHeaderFile includes decls =
    Syntax.Program {
      Syntax.programPreprocessorDirectives = [
        Syntax.PreprocessorDirectivePragma (Syntax.PragmaDirective {
          Syntax.pragmaDirectiveContent = "once"})],
      Syntax.programIncludes = includes,
      Syntax.programDeclarations = decls}

-- | Create an identifier expression
createIdentifierExpr :: String -> Syntax.Expression
createIdentifierExpr name = cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionIdentifier name)

-- | Create a lambda expression
createLambdaExpr :: [Syntax.Parameter] -> Syntax.Expression -> Syntax.Expression
createLambdaExpr params body =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionLambda (Syntax.LambdaExpression {
      Syntax.lambdaExpressionCaptures = (Syntax.CaptureListCaptures []),
      Syntax.lambdaExpressionParameters = params,
      Syntax.lambdaExpressionReturnType = Nothing,
      Syntax.lambdaExpressionBody = (Syntax.CompoundStatement [
        Syntax.StatementExpression body])})))

-- | Create a boolean literal expression
createLiteralBoolExpr :: Bool -> Syntax.Expression
createLiteralBoolExpr val =
    cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionLiteral (Syntax.LiteralBoolean (Syntax.BooleanLiteral val)))

-- | Create an integer literal expression
createLiteralIntExpr :: Integer -> Syntax.Expression
createLiteralIntExpr val =
    cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteralDecimal val)))

-- | Create a string literal expression
createLiteralStringExpr :: String -> Syntax.Expression
createLiteralStringExpr val =
    cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral val)))

-- | Create a member access expression
createMemberAccessExpr :: Syntax.Expression -> String -> Syntax.Expression
createMemberAccessExpr objExpr member =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionMemberAccess (Syntax.MemberAccessOperation {
      Syntax.memberAccessOperationObject = (extractPostfixExpression objExpr),
      Syntax.memberAccessOperationMember = member}))

-- | Create a qualified type with a qualifier
createQualifiedType :: Syntax.TypeExpression -> Syntax.TypeQualifier -> Syntax.TypeExpression
createQualifiedType baseType qualifier =
    Syntax.TypeExpressionQualified (Syntax.QualifiedType {
      Syntax.qualifiedTypeBaseType = baseType,
      Syntax.qualifiedTypeQualifier = qualifier})

-- | Create a reference type
createReferenceType :: Syntax.TypeExpression -> Syntax.TypeExpression
createReferenceType baseType = createQualifiedType baseType Syntax.TypeQualifierLvalueRef

-- | Create a return statement with a value
createReturnStmt :: Syntax.Expression -> Syntax.Statement
createReturnStmt expr = Syntax.StatementJump (Syntax.JumpStatementReturnValue expr)

-- | Create a void return statement
createReturnVoidStmt :: Syntax.Statement
createReturnVoidStmt = Syntax.StatementJump Syntax.JumpStatementReturnVoid

-- | Create a template type
createTemplateType :: String -> [Syntax.TypeExpression] -> Syntax.TypeExpression
createTemplateType name args =
    Syntax.TypeExpressionTemplate (Syntax.TemplateType {
      Syntax.templateTypeName = name,
      Syntax.templateTypeArguments = (Lists.map (\a -> Syntax.TemplateArgumentType a) args)})

-- | Create a *this expression
createThisExpr :: Syntax.Expression
createThisExpr =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "*this"))

-- | Create a throw statement
createThrowStmt :: String -> Syntax.Expression -> Syntax.Statement
createThrowStmt exceptionType arg =
    Syntax.StatementJump (Syntax.JumpStatementThrow (createFunctionCallExpr exceptionType [
      arg]))

-- | Create a typeid(...).name() call expression
createTypeIdNameCall :: Syntax.Expression
createTypeIdNameCall =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
      Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionMemberAccess (Syntax.MemberAccessOperation {
        Syntax.memberAccessOperationObject = (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
          Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "typeid")),
          Syntax.functionCallOperationArguments = [
            cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionParenthesized createThisExpr)]})),
        Syntax.memberAccessOperationMember = "name"})),
      Syntax.functionCallOperationArguments = []}))

-- | Create a type name expression
createTypeNameExpr :: String -> Syntax.Expression
createTypeNameExpr typeName =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier typeName))

-- | Create a variant expression
createVariantExpr :: Syntax.Expression
createVariantExpr =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "variant"))

-- | An empty function body
emptyFunctionBody :: Syntax.FunctionBody
emptyFunctionBody = Syntax.FunctionBodyCompound (Syntax.CompoundStatement [])

-- | Extract the PostfixExpression from a fully-wrapped Expression
extractPostfixExpression :: Syntax.Expression -> Syntax.PostfixExpression
extractPostfixExpression expr =
    case expr of
      Syntax.ExpressionAssignment v0 -> case v0 of
        Syntax.AssignmentExpressionConditional v1 -> case v1 of
          Syntax.ConditionalExpressionLogicalOr v2 -> case v2 of
            Syntax.LogicalOrExpressionLogicalAnd v3 -> case v3 of
              Syntax.LogicalAndExpressionInclusiveOr v4 -> case v4 of
                Syntax.InclusiveOrExpressionExclusiveOr v5 -> case v5 of
                  Syntax.ExclusiveOrExpressionAnd v6 -> case v6 of
                    Syntax.AndExpressionEquality v7 -> case v7 of
                      Syntax.EqualityExpressionRelational v8 -> case v8 of
                        Syntax.RelationalExpressionShift v9 -> case v9 of
                          Syntax.ShiftExpressionAdditive v10 -> case v10 of
                            Syntax.AdditiveExpressionMultiplicative v11 -> case v11 of
                              Syntax.MultiplicativeExpressionUnary v12 -> case v12 of
                                Syntax.UnaryExpressionPostfix v13 -> v13
                                _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                              _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                            _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                          _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                        _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                      _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                    _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                  _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
                _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
              _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
            _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
          _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
        _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")
      _ -> Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "error")

-- | Protected access specifier member specification
memberSpecificationProtected :: Syntax.MemberSpecification
memberSpecificationProtected = Syntax.MemberSpecificationAccessLabel Syntax.AccessSpecifierProtected

-- | Public access specifier member specification
memberSpecificationPublic :: Syntax.MemberSpecification
memberSpecificationPublic = Syntax.MemberSpecificationAccessLabel Syntax.AccessSpecifierPublic

-- | Create a string expression
stringExpression :: String -> Syntax.Expression
stringExpression s =
    cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral s)))

-- | Add const qualifier to a type
toConstType :: Syntax.TypeExpression -> Syntax.TypeExpression
toConstType baseType =
    Syntax.TypeExpressionQualified (Syntax.QualifiedType {
      Syntax.qualifiedTypeBaseType = baseType,
      Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierConst})

-- | Create an unnamed parameter
unnamedParameter :: String -> Syntax.TypeExpression -> Syntax.Parameter
unnamedParameter name typ =
    Syntax.Parameter {
      Syntax.parameterType = typ,
      Syntax.parameterName = name,
      Syntax.parameterUnnamed = True,
      Syntax.parameterDefaultValue = Nothing}
