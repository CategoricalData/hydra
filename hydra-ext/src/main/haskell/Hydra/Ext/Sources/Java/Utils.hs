-- | Java utilities for constructing Java syntax trees.
-- Provides functions for building common Java AST patterns.

module Hydra.Ext.Sources.Java.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S

-- Additional imports
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Ext.Java.Environment as JavaHelpers
import qualified Hydra.Ext.Dsl.Java.Syntax as JavaDsl
import qualified Hydra.Ext.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Ext.Sources.Java.Environment as JavaEnvironmentSource
import qualified Hydra.Ext.Sources.Java.Language as JavaLanguageSource
import qualified Hydra.Ext.Sources.Java.Names as JavaNamesSource
import qualified Hydra.Ext.Sources.Java.Serde as JavaSerdeSource


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.java.utils"

module_ :: Module
module_ = Module ns definitions
    [moduleNamespace JavaLanguageSource.module_, JavaNamesSource.ns, JavaSerdeSource.ns, Formatting.ns, Names.ns, Serialization.ns]
    (JavaEnvironmentSource.ns:JavaSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Java utilities for constructing Java syntax trees"
  where
    definitions = [
      toDefinition addExpressions,
      toDefinition addInScopeVar,
      toDefinition addInScopeVars,
      toDefinition addJavaTypeParameter,
      toDefinition addVarRename,
      toDefinition fieldExpression,
      toDefinition fieldNameToJavaExpression,
      toDefinition fieldNameToJavaIdentifier,
      toDefinition fieldNameToJavaVariableDeclarator,
      toDefinition fieldNameToJavaVariableDeclaratorId,
      toDefinition finalVarDeclarationStatement,
      toDefinition importAliasesForModule,
      toDefinition interfaceMethodDeclaration,
      toDefinition isEscaped,
      toDefinition javaAdditiveExpressionToJavaExpression,
      toDefinition javaArrayCreation,
      toDefinition javaArrayInitializer,
      toDefinition javaAssignmentStatement,
      toDefinition javaBoolean,
      toDefinition javaBooleanExpression,
      toDefinition javaBooleanType,
      toDefinition javaBytePrimitiveType,
      toDefinition javaCastExpression,
      toDefinition javaCastExpressionToJavaExpression,
      toDefinition javaCastPrimitive,
      toDefinition javaClassDeclaration,
      toDefinition javaClassType,
      toDefinition javaClassTypeToJavaType,
      toDefinition javaConditionalAndExpressionToJavaExpression,
      toDefinition javaConstructorCall,
      toDefinition javaConstructorName,
      toDefinition javaDeclName,
      toDefinition javaDoubleCastExpression,
      toDefinition javaDoubleCastExpressionToJavaExpression,
      toDefinition javaEmptyStatement,
      toDefinition javaEqualityExpressionToJavaExpression,
      toDefinition javaEqualityExpressionToJavaInclusiveOrExpression,
      toDefinition javaEquals,
      toDefinition javaEqualsNull,
      toDefinition javaExpressionNameToJavaExpression,
      toDefinition javaExpressionToJavaPrimary,
      toDefinition javaExpressionToJavaUnaryExpression,
      toDefinition javaFieldAccessToJavaExpression,
      toDefinition javaIdentifier,
      toDefinition javaIdentifierToJavaExpression,
      toDefinition javaIdentifierToJavaExpressionName,
      toDefinition javaIdentifierToJavaRelationalExpression,
      toDefinition javaIdentifierToJavaUnaryExpression,
      toDefinition javaInstanceOf,
      toDefinition javaInt,
      toDefinition javaIntExpression,
      toDefinition javaIntType,
      toDefinition javaInterfaceDeclarationToJavaClassBodyDeclaration,
      toDefinition javaLambda,
      toDefinition javaLambdaFromBlock,
      toDefinition javaLiteralToJavaExpression,
      toDefinition javaLiteralToJavaMultiplicativeExpression,
      toDefinition javaLiteralToJavaPrimary,
      toDefinition javaLiteralToJavaRelationalExpression,
      toDefinition javaMemberField,
      toDefinition javaMethodBody,
      toDefinition javaMethodDeclarationToJavaClassBodyDeclaration,
      toDefinition javaMethodHeader,
      toDefinition javaMethodInvocationToJavaExpression,
      toDefinition javaMethodInvocationToJavaPostfixExpression,
      toDefinition javaMethodInvocationToJavaPrimary,
      toDefinition javaMethodInvocationToJavaStatement,
      toDefinition javaMultiplicativeExpressionToJavaRelationalExpression,
      toDefinition javaPackageDeclaration,
      toDefinition javaPostfixExpressionToJavaEqualityExpression,
      toDefinition javaPostfixExpressionToJavaExpression,
      toDefinition javaPostfixExpressionToJavaInclusiveOrExpression,
      toDefinition javaPostfixExpressionToJavaRelationalExpression,
      toDefinition javaPostfixExpressionToJavaUnaryExpression,
      toDefinition javaPrimaryToJavaExpression,
      toDefinition javaPrimaryToJavaUnaryExpression,
      toDefinition javaPrimitiveTypeToJavaType,
      toDefinition javaRefType,
      toDefinition javaReferenceTypeToRawType,
      toDefinition javaRelationalExpressionToJavaEqualityExpression,
      toDefinition javaRelationalExpressionToJavaExpression,
      toDefinition javaRelationalExpressionToJavaUnaryExpression,
      toDefinition javaReturnStatement,
      toDefinition javaStatementsToBlock,
      toDefinition javaString,
      toDefinition javaStringMultiplicativeExpression,
      toDefinition javaThis,
      toDefinition javaThrowIllegalArgumentException,
      toDefinition javaThrowIllegalStateException,
      toDefinition javaThrowStatement,
      toDefinition javaTypeFromTypeName,
      toDefinition javaTypeIdentifier,
      toDefinition javaTypeIdentifierToJavaTypeArgument,
      toDefinition javaTypeName,
      toDefinition javaTypeParameter,
      toDefinition javaTypeToJavaFormalParameter,
      toDefinition javaTypeToJavaReferenceType,
      toDefinition javaTypeToJavaResult,
      toDefinition javaTypeToJavaTypeArgument,
      toDefinition javaTypeVariable,
      toDefinition javaTypeVariableToType,
      toDefinition javaUnaryExpressionToJavaExpression,
      toDefinition javaUnaryExpressionToJavaRelationalExpression,
      toDefinition javaVariableDeclarator,
      toDefinition javaVariableDeclaratorId,
      toDefinition javaVariableName,
      toDefinition lookupJavaVarName,
      toDefinition makeConstructor,
      toDefinition methodDeclaration,
      toDefinition methodInvocation,
      toDefinition methodInvocationStatic,
      toDefinition methodInvocationStaticWithTypeArgs,
      toDefinition nameToJavaClassType,
      toDefinition nameToJavaName,
      toDefinition nameToJavaReferenceType,
      toDefinition nameToJavaTypeIdentifier,
      toDefinition nameToQualifiedJavaName,
      toDefinition overrideAnnotation,
      toDefinition referenceTypeToResult,
      toDefinition sanitizeJavaName,
      toDefinition suppressWarningsUncheckedAnnotation,
      toDefinition toAcceptMethod,
      toDefinition toAssignStmt,
      toDefinition toJavaArrayType,
      toDefinition typeParameterToReferenceType,
      toDefinition typeParameterToTypeArgument,
      toDefinition unTypeParameter,
      toDefinition unescape,
      toDefinition uniqueVarName,
      toDefinition uniqueVarName_go,
      toDefinition varDeclarationStatement,
      toDefinition variantClassName,
      toDefinition variableDeclarationStatement,
      toDefinition variableToJavaIdentifier,
      toDefinition visitorTypeVariable]


javaIdentifier :: TTermDefinition (String -> Java.Identifier)
javaIdentifier = def "javaIdentifier" $
  lambda "s" $ JavaDsl.identifier (sanitizeJavaName @@ var "s")

javaTypeIdentifier :: TTermDefinition (String -> Java.TypeIdentifier)
javaTypeIdentifier = def "javaTypeIdentifier" $
  lambda "s" $ JavaDsl.typeIdentifier (JavaDsl.identifier (var "s"))

javaTypeName :: TTermDefinition (Java.Identifier -> Java.TypeName)
javaTypeName = def "javaTypeName" $
  lambda "id" $ JavaDsl.typeName (JavaDsl.typeIdentifier (var "id")) nothing

javaDeclName :: TTermDefinition (Name -> Java.TypeIdentifier)
javaDeclName = def "javaDeclName" $
  lambda "name" $ JavaDsl.typeIdentifier (javaVariableName @@ var "name")

javaVariableName :: TTermDefinition (Name -> Java.Identifier)
javaVariableName = def "javaVariableName" $
  lambda "name" $ javaIdentifier @@ (Names.localNameOf @@ var "name")

javaVariableDeclaratorId :: TTermDefinition (Java.Identifier -> Java.VariableDeclaratorId)
javaVariableDeclaratorId = def "javaVariableDeclaratorId" $
  lambda "id" $ JavaDsl.variableDeclaratorId (var "id") nothing

javaVariableDeclarator :: TTermDefinition (Java.Identifier -> Maybe Java.VariableInitializer -> Java.VariableDeclarator)
javaVariableDeclarator = def "javaVariableDeclarator" $
  lambda "id" $ lambda "minit" $ JavaDsl.variableDeclarator (javaVariableDeclaratorId @@ var "id") (var "minit")


javaBoolean :: TTermDefinition (Bool -> Java.Literal)
javaBoolean = def "javaBoolean" $
  lambda "b" $ JavaDsl.literalBoolean (var "b")

javaInt :: TTermDefinition (Int -> Java.Literal)
javaInt = def "javaInt" $
  lambda "i" $ JavaDsl.literalInteger (JavaDsl.integerLiteral (var "i"))

javaString :: TTermDefinition (String -> Java.Literal)
javaString = def "javaString" $
  lambda "s" $ JavaDsl.literalString (JavaDsl.stringLiteral (var "s"))


javaLiteralToJavaPrimary :: TTermDefinition (Java.Literal -> Java.Primary)
javaLiteralToJavaPrimary = def "javaLiteralToJavaPrimary" $
  lambda "lit" $ JavaDsl.literalToPrimary (var "lit")

javaExpressionToJavaPrimary :: TTermDefinition (Java.Expression -> Java.Primary)
javaExpressionToJavaPrimary = def "javaExpressionToJavaPrimary" $
  doc "Convert an Expression to a Primary, avoiding unnecessary parentheses when the expression is already a simple primary chain" $
  lambda "e" $
    -- Try to unwrap Expression -> AssignmentExpression(conditional) -> ConditionalExpression(simple) -> ...
    -- If the expression is just a wrapped Primary, extract it; otherwise wrap in parens
    "fallback" <~ JavaDsl.expressionToPrimary (var "e") $
    cases Java._Expression (var "e") (Just $ var "fallback") [
      Java._Expression_assignment>>: "ae" ~>
        cases Java._AssignmentExpression (var "ae") (Just $ var "fallback") [
          Java._AssignmentExpression_conditional>>: "ce" ~>
            cases Java._ConditionalExpression (var "ce") (Just $ var "fallback") [
              Java._ConditionalExpression_simple>>: "cor" ~>
                "cands" <~ unwrap Java._ConditionalOrExpression @@ var "cor" $
                Logic.ifElse (Equality.equal (Lists.length $ var "cands") (int32 1))
                  ("iors" <~ unwrap Java._ConditionalAndExpression @@ (Lists.head $ var "cands") $
                  Logic.ifElse (Equality.equal (Lists.length $ var "iors") (int32 1))
                    ("xors" <~ unwrap Java._InclusiveOrExpression @@ (Lists.head $ var "iors") $
                    Logic.ifElse (Equality.equal (Lists.length $ var "xors") (int32 1))
                      ("ands" <~ unwrap Java._ExclusiveOrExpression @@ (Lists.head $ var "xors") $
                      Logic.ifElse (Equality.equal (Lists.length $ var "ands") (int32 1))
                        ("eqs" <~ unwrap Java._AndExpression @@ (Lists.head $ var "ands") $
                        Logic.ifElse (Equality.equal (Lists.length $ var "eqs") (int32 1))
                        (cases Java._EqualityExpression (Lists.head $ var "eqs") (Just $ var "fallback") [
                          Java._EqualityExpression_unary>>: "rel" ~>
                            cases Java._RelationalExpression (var "rel") (Just $ var "fallback") [
                              Java._RelationalExpression_simple>>: "shift" ~>
                                cases Java._ShiftExpression (var "shift") (Just $ var "fallback") [
                                  Java._ShiftExpression_unary>>: "add" ~>
                                    cases Java._AdditiveExpression (var "add") (Just $ var "fallback") [
                                      Java._AdditiveExpression_unary>>: "mul" ~>
                                        cases Java._MultiplicativeExpression (var "mul") (Just $ var "fallback") [
                                          Java._MultiplicativeExpression_unary>>: "unary" ~>
                                            cases Java._UnaryExpression (var "unary") (Just $ var "fallback") [
                                              Java._UnaryExpression_other>>: "npm" ~>
                                                cases Java._UnaryExpressionNotPlusMinus (var "npm") (Just $ var "fallback") [
                                                  Java._UnaryExpressionNotPlusMinus_postfix>>: "pf" ~>
                                                    cases Java._PostfixExpression (var "pf") (Just $ var "fallback") [
                                                      Java._PostfixExpression_primary>>: "p" ~> var "p"]]]]]]]])
                        (var "fallback"))
                        (var "fallback"))
                      (var "fallback"))
                    (var "fallback"))
                  (var "fallback")]]]

javaPrimaryToJavaUnaryExpression :: TTermDefinition (Java.Primary -> Java.UnaryExpression)
javaPrimaryToJavaUnaryExpression = def "javaPrimaryToJavaUnaryExpression" $
  lambda "p" $ JavaDsl.unaryExpressionOther
    (JavaDsl.unaryExpressionNotPlusMinusPostfix
      (JavaDsl.postfixExpressionPrimary (var "p")))

javaPrimaryToJavaExpression :: TTermDefinition (Java.Primary -> Java.Expression)
javaPrimaryToJavaExpression = def "javaPrimaryToJavaExpression" $
  lambda "p" $ JavaDsl.primaryToExpression (var "p")

javaPostfixExpressionToJavaUnaryExpression :: TTermDefinition (Java.PostfixExpression -> Java.UnaryExpression)
javaPostfixExpressionToJavaUnaryExpression = def "javaPostfixExpressionToJavaUnaryExpression" $
  lambda "pe" $ JavaDsl.unaryExpressionOther
    (JavaDsl.unaryExpressionNotPlusMinusPostfix (var "pe"))

javaPostfixExpressionToJavaExpression :: TTermDefinition (Java.PostfixExpression -> Java.Expression)
javaPostfixExpressionToJavaExpression = def "javaPostfixExpressionToJavaExpression" $
  lambda "pe" $ JavaDsl.postfixToExpression (var "pe")

javaPostfixExpressionToJavaRelationalExpression :: TTermDefinition (Java.PostfixExpression -> Java.RelationalExpression)
javaPostfixExpressionToJavaRelationalExpression = def "javaPostfixExpressionToJavaRelationalExpression" $
  lambda "pe" $ JavaDsl.postfixToRelationalExpression (var "pe")

javaUnaryExpressionToJavaRelationalExpression :: TTermDefinition (Java.UnaryExpression -> Java.RelationalExpression)
javaUnaryExpressionToJavaRelationalExpression = def "javaUnaryExpressionToJavaRelationalExpression" $
  lambda "ue" $ JavaDsl.relationalExpressionSimple
    (JavaDsl.shiftExpressionUnary
      (JavaDsl.additiveExpressionUnary
        (JavaDsl.multiplicativeExpressionUnary (var "ue"))))

javaUnaryExpressionToJavaExpression :: TTermDefinition (Java.UnaryExpression -> Java.Expression)
javaUnaryExpressionToJavaExpression = def "javaUnaryExpressionToJavaExpression" $
  lambda "ue" $ JavaDsl.unaryToExpression (var "ue")

javaRelationalExpressionToJavaExpression :: TTermDefinition (Java.RelationalExpression -> Java.Expression)
javaRelationalExpressionToJavaExpression = def "javaRelationalExpressionToJavaExpression" $
  lambda "re" $ javaEqualityExpressionToJavaExpression @@
    (JavaDsl.equalityExpressionUnary (var "re"))

javaRelationalExpressionToJavaEqualityExpression :: TTermDefinition (Java.RelationalExpression -> Java.EqualityExpression)
javaRelationalExpressionToJavaEqualityExpression = def "javaRelationalExpressionToJavaEqualityExpression" $
  lambda "re" $ JavaDsl.equalityExpressionUnary (var "re")

javaRelationalExpressionToJavaUnaryExpression :: TTermDefinition (Java.RelationalExpression -> Java.UnaryExpression)
javaRelationalExpressionToJavaUnaryExpression = def "javaRelationalExpressionToJavaUnaryExpression" $
  lambda "re" $ JavaDsl.relationalToUnary (var "re")

javaMultiplicativeExpressionToJavaRelationalExpression :: TTermDefinition (Java.MultiplicativeExpression -> Java.RelationalExpression)
javaMultiplicativeExpressionToJavaRelationalExpression = def "javaMultiplicativeExpressionToJavaRelationalExpression" $
  lambda "me" $ JavaDsl.relationalExpressionSimple
    (JavaDsl.shiftExpressionUnary
      (JavaDsl.additiveExpressionUnary (var "me")))

javaLiteralToJavaMultiplicativeExpression :: TTermDefinition (Java.Literal -> Java.MultiplicativeExpression)
javaLiteralToJavaMultiplicativeExpression = def "javaLiteralToJavaMultiplicativeExpression" $
  lambda "lit" $ JavaDsl.literalToMultiplicativeExpression (var "lit")

javaLiteralToJavaRelationalExpression :: TTermDefinition (Java.Literal -> Java.RelationalExpression)
javaLiteralToJavaRelationalExpression = def "javaLiteralToJavaRelationalExpression" $
  lambda "lit" $ JavaDsl.literalToRelationalExpression (var "lit")

javaLiteralToJavaExpression :: TTermDefinition (Java.Literal -> Java.Expression)
javaLiteralToJavaExpression = def "javaLiteralToJavaExpression" $
  lambda "lit" $ JavaDsl.literalToExpression (var "lit")

javaIdentifierToJavaExpressionName :: TTermDefinition (Java.Identifier -> Java.ExpressionName)
javaIdentifierToJavaExpressionName = def "javaIdentifierToJavaExpressionName" $
  lambda "id" $ JavaDsl.expressionName nothing (var "id")

javaIdentifierToJavaExpression :: TTermDefinition (Java.Identifier -> Java.Expression)
javaIdentifierToJavaExpression = def "javaIdentifierToJavaExpression" $
  lambda "id" $ JavaDsl.identifierToExpression (var "id")

javaIdentifierToJavaRelationalExpression :: TTermDefinition (Java.Identifier -> Java.RelationalExpression)
javaIdentifierToJavaRelationalExpression = def "javaIdentifierToJavaRelationalExpression" $
  lambda "id" $ JavaDsl.identifierToRelationalExpression (var "id")

javaIdentifierToJavaUnaryExpression :: TTermDefinition (Java.Identifier -> Java.UnaryExpression)
javaIdentifierToJavaUnaryExpression = def "javaIdentifierToJavaUnaryExpression" $
  lambda "id" $ JavaDsl.identifierToUnary (var "id")

javaExpressionNameToJavaExpression :: TTermDefinition (Java.ExpressionName -> Java.Expression)
javaExpressionNameToJavaExpression = def "javaExpressionNameToJavaExpression" $
  lambda "en" $ JavaDsl.expressionNameToExpression (var "en")

javaFieldAccessToJavaExpression :: TTermDefinition (Java.FieldAccess -> Java.Expression)
javaFieldAccessToJavaExpression = def "javaFieldAccessToJavaExpression" $
  lambda "fa" $ JavaDsl.fieldAccessToExpression (var "fa")

javaCastExpressionToJavaExpression :: TTermDefinition (Java.CastExpression -> Java.Expression)
javaCastExpressionToJavaExpression = def "javaCastExpressionToJavaExpression" $
  lambda "ce" $ JavaDsl.castExpressionToExpression (var "ce")

javaMethodInvocationToJavaPrimary :: TTermDefinition (Java.MethodInvocation -> Java.Primary)
javaMethodInvocationToJavaPrimary = def "javaMethodInvocationToJavaPrimary" $
  lambda "mi" $ JavaDsl.methodInvocationToPrimary (var "mi")

javaMethodInvocationToJavaExpression :: TTermDefinition (Java.MethodInvocation -> Java.Expression)
javaMethodInvocationToJavaExpression = def "javaMethodInvocationToJavaExpression" $
  lambda "mi" $ JavaDsl.methodInvocationToExpression (var "mi")

javaMethodInvocationToJavaPostfixExpression :: TTermDefinition (Java.MethodInvocation -> Java.PostfixExpression)
javaMethodInvocationToJavaPostfixExpression = def "javaMethodInvocationToJavaPostfixExpression" $
  lambda "mi" $ JavaDsl.methodInvocationToPostfix (var "mi")

javaMethodInvocationToJavaStatement :: TTermDefinition (Java.MethodInvocation -> Java.Statement)
javaMethodInvocationToJavaStatement = def "javaMethodInvocationToJavaStatement" $
  lambda "mi" $ JavaDsl.methodInvocationToStatement (var "mi")

javaConditionalAndExpressionToJavaExpression :: TTermDefinition (Java.ConditionalAndExpression -> Java.Expression)
javaConditionalAndExpressionToJavaExpression = def "javaConditionalAndExpressionToJavaExpression" $
  lambda "cae" $ JavaDsl.conditionalAndToExpression (var "cae")

javaEqualityExpressionToJavaInclusiveOrExpression :: TTermDefinition (Java.EqualityExpression -> Java.InclusiveOrExpression)
javaEqualityExpressionToJavaInclusiveOrExpression = def "javaEqualityExpressionToJavaInclusiveOrExpression" $
  lambda "ee" $ JavaDsl.equalityToInclusiveOr (var "ee")

javaEqualityExpressionToJavaExpression :: TTermDefinition (Java.EqualityExpression -> Java.Expression)
javaEqualityExpressionToJavaExpression = def "javaEqualityExpressionToJavaExpression" $
  lambda "ee" $ JavaDsl.equalityToExpression (var "ee")

javaPostfixExpressionToJavaEqualityExpression :: TTermDefinition (Java.PostfixExpression -> Java.EqualityExpression)
javaPostfixExpressionToJavaEqualityExpression = def "javaPostfixExpressionToJavaEqualityExpression" $
  lambda "pe" $ JavaDsl.equalityExpressionUnary
    (JavaDsl.postfixToRelationalExpression (var "pe"))

javaPostfixExpressionToJavaInclusiveOrExpression :: TTermDefinition (Java.PostfixExpression -> Java.InclusiveOrExpression)
javaPostfixExpressionToJavaInclusiveOrExpression = def "javaPostfixExpressionToJavaInclusiveOrExpression" $
  lambda "pe" $ JavaDsl.postfixToInclusiveOr (var "pe")

javaAdditiveExpressionToJavaExpression :: TTermDefinition (Java.AdditiveExpression -> Java.Expression)
javaAdditiveExpressionToJavaExpression = def "javaAdditiveExpressionToJavaExpression" $
  lambda "ae" $ JavaDsl.additiveToExpression (var "ae")

javaExpressionToJavaUnaryExpression :: TTermDefinition (Java.Expression -> Java.UnaryExpression)
javaExpressionToJavaUnaryExpression = def "javaExpressionToJavaUnaryExpression" $
  lambda "e" $ JavaDsl.expressionToUnary (var "e")


javaPrimitiveTypeToJavaType :: TTermDefinition (Java.PrimitiveType -> Java.Type)
javaPrimitiveTypeToJavaType = def "javaPrimitiveTypeToJavaType" $
  lambda "pt" $ JavaDsl.typePrimitive
    (JavaDsl.primitiveTypeWithAnnotations (var "pt") (list ([] :: [TTerm Java.Annotation])))

javaClassTypeToJavaType :: TTermDefinition (Java.ClassType -> Java.Type)
javaClassTypeToJavaType = def "javaClassTypeToJavaType" $
  lambda "ct" $ JavaDsl.typeReference
    (JavaDsl.referenceTypeClassOrInterface
      (JavaDsl.classOrInterfaceTypeClass (var "ct")))

javaTypeVariableToType :: TTermDefinition (Java.TypeVariable -> Java.Type)
javaTypeVariableToType = def "javaTypeVariableToType" $
  lambda "tv" $ JavaDsl.typeReference (JavaDsl.referenceTypeVariable (var "tv"))

javaRefType :: TTermDefinition ([Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.Type)
javaRefType = def "javaRefType" $
  lambda "args" $ lambda "pkg" $ lambda "id" $
    JavaDsl.typeReference
      (JavaDsl.referenceTypeClassOrInterface
        (JavaDsl.classOrInterfaceTypeClass
          (javaClassType @@ var "args" @@ var "pkg" @@ var "id")))

javaClassType :: TTermDefinition ([Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.ClassType)
javaClassType = def "javaClassType" $
  lambda "args" $ lambda "pkg" $ lambda "id" $ lets [
    "qual">: Maybes.cases (var "pkg")
      JavaDsl.classTypeQualifierNone
      (lambda "p" $ JavaDsl.classTypeQualifierPackage (var "p")),
    "targs">: Lists.map
      (lambda "rt" $ JavaDsl.typeArgumentReference (var "rt"))
      (var "args")] $
    JavaDsl.classType (list ([] :: [TTerm Java.Annotation])) (var "qual")
      (javaTypeIdentifier @@ var "id") (var "targs")

javaTypeVariable :: TTermDefinition (String -> Java.ReferenceType)
javaTypeVariable = def "javaTypeVariable" $
  lambda "v" $ JavaDsl.referenceTypeVariable
    (JavaDsl.typeVariable (list ([] :: [TTerm Java.Annotation]))
      (javaTypeIdentifier @@ (Formatting.capitalize @@ var "v")))

javaBooleanType :: TTermDefinition Java.Type
javaBooleanType = def "javaBooleanType" $
  javaPrimitiveTypeToJavaType @@ JavaDsl.primitiveTypeBoolean

javaIntType :: TTermDefinition Java.Type
javaIntType = def "javaIntType" $
  javaPrimitiveTypeToJavaType @@
    JavaDsl.primitiveTypeNumeric (JavaDsl.numericTypeIntegral JavaDsl.integralTypeInt)


javaBooleanExpression :: TTermDefinition (Bool -> Java.Expression)
javaBooleanExpression = def "javaBooleanExpression" $
  lambda "b" $ javaPrimaryToJavaExpression @@ (javaLiteralToJavaPrimary @@ (javaBoolean @@ var "b"))

javaIntExpression :: TTermDefinition (Int -> Java.Expression)
javaIntExpression = def "javaIntExpression" $
  lambda "i" $ javaPrimaryToJavaExpression @@ (javaLiteralToJavaPrimary @@ (javaInt @@ var "i"))


javaCastExpression :: TTermDefinition (Java.ReferenceType -> Java.UnaryExpression -> Java.CastExpression)
javaCastExpression = def "javaCastExpression" $
  lambda "rt" $ lambda "expr" $
    JavaDsl.castExpressionNotPlusMinus
      (JavaDsl.castExpressionNotPlusMinus_
        (JavaDsl.castExpressionRefAndBounds (var "rt") (list ([] :: [TTerm Java.AdditionalBound])))
        (var "expr"))

javaCastPrimitive :: TTermDefinition (Java.PrimitiveType -> Java.UnaryExpression -> Java.CastExpression)
javaCastPrimitive = def "javaCastPrimitive" $
  lambda "pt" $ lambda "expr" $
    JavaDsl.castExpressionPrimitive
      (JavaDsl.castExpressionPrimitive_
        (JavaDsl.primitiveTypeWithAnnotations (var "pt") (list ([] :: [TTerm Java.Annotation])))
        (var "expr"))


javaReturnStatement :: TTermDefinition (Maybe Java.Expression -> Java.Statement)
javaReturnStatement = def "javaReturnStatement" $
  lambda "mex" $ JavaDsl.statementWithoutTrailing
    (JavaDsl.stmtReturn (JavaDsl.returnStatement (var "mex")))

javaThrowStatement :: TTermDefinition (Java.Expression -> Java.Statement)
javaThrowStatement = def "javaThrowStatement" $
  lambda "e" $ JavaDsl.statementWithoutTrailing
    (JavaDsl.stmtThrow (JavaDsl.throwStatement (var "e")))

javaEmptyStatement :: TTermDefinition Java.Statement
javaEmptyStatement = def "javaEmptyStatement" $
  JavaDsl.statementWithoutTrailing JavaDsl.stmtEmpty

javaAssignmentStatement :: TTermDefinition (Java.LeftHandSide -> Java.Expression -> Java.Statement)
javaAssignmentStatement = def "javaAssignmentStatement" $
  lambda "lhs" $ lambda "rhs" $
    JavaDsl.statementWithoutTrailing
      (JavaDsl.stmtExpression
        (JavaDsl.expressionStatement
          (JavaDsl.stmtExprAssignment
            (JavaDsl.assignment (var "lhs") JavaDsl.assignmentOperatorSimple (var "rhs")))))

javaStatementsToBlock :: TTermDefinition ([Java.Statement] -> Java.Block)
javaStatementsToBlock = def "javaStatementsToBlock" $
  lambda "stmts" $ JavaDsl.block
    (Lists.map (lambda "s" $ JavaDsl.blockStatementStatement (var "s")) (var "stmts"))


javaLambda :: TTermDefinition (Name -> Java.Expression -> Java.Expression)
javaLambda = def "javaLambda" $
  lambda "v" $ lambda "body" $
    JavaDsl.expressionLambda
      (JavaDsl.lambdaExpression
        (JavaDsl.lambdaParametersSingle (variableToJavaIdentifier @@ var "v"))
        (JavaDsl.lambdaBodyExpression (var "body")))

javaLambdaFromBlock :: TTermDefinition (Name -> Java.Block -> Java.Expression)
javaLambdaFromBlock = def "javaLambdaFromBlock" $
  lambda "v" $ lambda "block" $
    JavaDsl.expressionLambda
      (JavaDsl.lambdaExpression
        (JavaDsl.lambdaParametersSingle (variableToJavaIdentifier @@ var "v"))
        (JavaDsl.lambdaBodyBlock (var "block")))


javaMethodBody :: TTermDefinition (Maybe [Java.BlockStatement] -> Java.MethodBody)
javaMethodBody = def "javaMethodBody" $
  lambda "mstmts" $ Maybes.cases (var "mstmts")
    JavaDsl.methodBodyNone
    (lambda "stmts" $ JavaDsl.methodBodyBlock (JavaDsl.block (var "stmts")))

javaMethodHeader :: TTermDefinition ([Java.TypeParameter] -> String -> [Java.FormalParameter] -> Java.Result -> Java.MethodHeader)
javaMethodHeader = def "javaMethodHeader" $
  lambda "tparams" $ lambda "methodName" $ lambda "params" $ lambda "result" $
    JavaDsl.methodHeader (var "tparams") (var "result")
      (JavaDsl.methodDeclarator (JavaDsl.identifier (var "methodName")) nothing (var "params"))
      nothing

javaMethodDeclarationToJavaClassBodyDeclaration :: TTermDefinition (Java.MethodDeclaration -> Java.ClassBodyDeclaration)
javaMethodDeclarationToJavaClassBodyDeclaration = def "javaMethodDeclarationToJavaClassBodyDeclaration" $
  lambda "md" $ JavaDsl.classBodyDeclClassMember (JavaDsl.classMemberDeclMethod (var "md"))

javaInterfaceDeclarationToJavaClassBodyDeclaration :: TTermDefinition (Java.NormalInterfaceDeclaration -> Java.ClassBodyDeclaration)
javaInterfaceDeclarationToJavaClassBodyDeclaration = def "javaInterfaceDeclarationToJavaClassBodyDeclaration" $
  lambda "nid" $ JavaDsl.classBodyDeclClassMember
    (JavaDsl.classMemberDeclInterface
      (JavaDsl.interfaceDeclarationNormalInterface (var "nid")))

javaMemberField :: TTermDefinition ([Java.FieldModifier] -> Java.Type -> Java.VariableDeclarator -> Java.ClassBodyDeclaration)
javaMemberField = def "javaMemberField" $
  lambda "mods" $ lambda "jt" $ lambda "v" $
    JavaDsl.classBodyDeclClassMember
      (JavaDsl.classMemberDeclField
        (JavaDsl.fieldDeclaration (var "mods") (JavaDsl.unannType (var "jt")) (list [var "v"])))

javaTypeToJavaFormalParameter :: TTermDefinition (Java.Type -> Name -> Java.FormalParameter)
javaTypeToJavaFormalParameter = def "javaTypeToJavaFormalParameter" $
  lambda "jt" $ lambda "fname" $
    JavaDsl.formalParameterSimple
      (JavaDsl.formalParameterSimple_ (list ([] :: [TTerm Java.VariableModifier]))
        (JavaDsl.unannType (var "jt"))
        (fieldNameToJavaVariableDeclaratorId @@ var "fname"))

javaTypeToJavaResult :: TTermDefinition (Java.Type -> Java.Result)
javaTypeToJavaResult = def "javaTypeToJavaResult" $
  lambda "jt" $ JavaDsl.resultType (JavaDsl.unannType (var "jt"))

javaTypeToJavaTypeArgument :: TTermDefinition (Java.Type -> Java.TypeArgument)
javaTypeToJavaTypeArgument = def "javaTypeToJavaTypeArgument" $
  lambda "t" $ cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt" $ JavaDsl.typeArgumentReference (var "rt"),
    Java._Type_primitive>>: constant $
      JavaDsl.typeArgumentWildcard
        (JavaDsl.wildcard (list ([] :: [TTerm Java.Annotation])) nothing)]

referenceTypeToResult :: TTermDefinition (Java.ReferenceType -> Java.Result)
referenceTypeToResult = def "referenceTypeToResult" $
  lambda "rt" $ javaTypeToJavaResult @@ JavaDsl.typeReference (var "rt")


javaConstructorName :: TTermDefinition (Java.Identifier -> Maybe Java.TypeArgumentsOrDiamond -> Java.ClassOrInterfaceTypeToInstantiate)
javaConstructorName = def "javaConstructorName" $
  lambda "id" $ lambda "targs" $
    JavaDsl.classOrInterfaceTypeToInstantiate
      (list [JavaDsl.annotatedIdentifier (list ([] :: [TTerm Java.Annotation])) (var "id")])
      (var "targs")

javaConstructorCall :: TTermDefinition (Java.ClassOrInterfaceTypeToInstantiate -> [Java.Expression] -> Maybe Java.ClassBody -> Java.Expression)
javaConstructorCall = def "javaConstructorCall" $
  lambda "ci" $ lambda "args" $ lambda "mbody" $
    JavaDsl.primaryToExpression
      (JavaDsl.primaryNoNewArray
        (JavaDsl.primaryClassInstance
          (JavaDsl.classInstanceCreationExpression nothing
            (JavaDsl.unqualifiedClassInstanceCreationExpression
              (list ([] :: [TTerm Java.TypeArgument])) (var "ci") (var "args") (var "mbody")))))


javaThis :: TTermDefinition Java.Expression
javaThis = def "javaThis" $
  JavaDsl.primaryToExpression
    (JavaDsl.primaryNoNewArray JavaDsl.primaryThis)


javaTypeParameter :: TTermDefinition (String -> Java.TypeParameter)
javaTypeParameter = def "javaTypeParameter" $
  lambda "v" $ JavaDsl.typeParameter
    (list ([] :: [TTerm Java.TypeParameterModifier]))
    (javaTypeIdentifier @@ var "v")
    nothing

javaTypeIdentifierToJavaTypeArgument :: TTermDefinition (Java.TypeIdentifier -> Java.TypeArgument)
javaTypeIdentifierToJavaTypeArgument = def "javaTypeIdentifierToJavaTypeArgument" $
  lambda "id" $ JavaDsl.typeArgumentReference
    (JavaDsl.referenceTypeVariable
      (JavaDsl.typeVariable (list ([] :: [TTerm Java.Annotation])) (var "id")))

typeParameterToTypeArgument :: TTermDefinition (Java.TypeParameter -> Java.TypeArgument)
typeParameterToTypeArgument = def "typeParameterToTypeArgument" $
  lambda "tp" $ javaTypeIdentifierToJavaTypeArgument @@
    (JavaDsl.typeParameterIdentifier (var "tp"))

typeParameterToReferenceType :: TTermDefinition (Java.TypeParameter -> Java.ReferenceType)
typeParameterToReferenceType = def "typeParameterToReferenceType" $
  lambda "tp" $ javaTypeVariable @@
    (JavaDsl.unIdentifier
      (JavaDsl.unTypeIdentifier
        (JavaDsl.typeParameterIdentifier (var "tp"))))


fieldNameToJavaIdentifier :: TTermDefinition (Name -> Java.Identifier)
fieldNameToJavaIdentifier = def "fieldNameToJavaIdentifier" $
  lambda "fname" $ javaIdentifier @@ (Core.unName $ var "fname")

fieldNameToJavaExpression :: TTermDefinition (Name -> Java.Expression)
fieldNameToJavaExpression = def "fieldNameToJavaExpression" $
  lambda "fname" $ JavaDsl.postfixToExpression
    (JavaDsl.postfixExpressionName
      (javaIdentifierToJavaExpressionName @@ (fieldNameToJavaIdentifier @@ var "fname")))

fieldNameToJavaVariableDeclaratorId :: TTermDefinition (Name -> Java.VariableDeclaratorId)
fieldNameToJavaVariableDeclaratorId = def "fieldNameToJavaVariableDeclaratorId" $
  lambda "fname" $ javaVariableDeclaratorId @@ (javaIdentifier @@ (Core.unName $ var "fname"))

fieldNameToJavaVariableDeclarator :: TTermDefinition (Name -> Java.VariableDeclarator)
fieldNameToJavaVariableDeclarator = def "fieldNameToJavaVariableDeclarator" $
  lambda "fname" $ javaVariableDeclarator @@ (javaIdentifier @@ (Core.unName $ var "fname")) @@ nothing

fieldExpression :: TTermDefinition (Java.Identifier -> Java.Identifier -> Java.ExpressionName)
fieldExpression = def "fieldExpression" $
  lambda "varId" $ lambda "fieldId" $
    JavaDsl.expressionName (just (JavaDsl.ambiguousName (list [var "varId"]))) (var "fieldId")


variableToJavaIdentifier :: TTermDefinition (Name -> Java.Identifier)
variableToJavaIdentifier = def "variableToJavaIdentifier" $
  lambda "name" $ lets [
    "v">: Core.unName $ var "name"] $
    Logic.ifElse (Equality.equal (var "v") (string "_"))
      (JavaDsl.identifier (string "ignored"))
      (JavaDsl.identifier (sanitizeJavaName @@ var "v"))

varDeclarationStatement :: TTermDefinition (Java.Identifier -> Java.Expression -> Java.BlockStatement)
varDeclarationStatement = def "varDeclarationStatement" $
  lambda "id" $ lambda "rhs" $
    JavaDsl.blockStatementLocalVariableDeclaration
      (JavaDsl.localVariableDeclarationStatement
        (JavaDsl.localVariableDeclaration
          (list ([] :: [TTerm Java.VariableModifier]))
          JavaDsl.localVariableTypeVar
          (list [javaVariableDeclarator @@ var "id" @@
            (just $ JavaDsl.variableInitializerExpression (var "rhs"))])))

-- | Like varDeclarationStatement, but with 'final' modifier.
--   Used in TCO while loops to create effectively-final snapshots of loop variables.
finalVarDeclarationStatement :: TTermDefinition (Java.Identifier -> Java.Expression -> Java.BlockStatement)
finalVarDeclarationStatement = def "finalVarDeclarationStatement" $
  lambda "id" $ lambda "rhs" $
    JavaDsl.blockStatementLocalVariableDeclaration
      (JavaDsl.localVariableDeclarationStatement
        (JavaDsl.localVariableDeclaration
          (list [inject Java._VariableModifier Java._VariableModifier_final unit])
          JavaDsl.localVariableTypeVar
          (list [javaVariableDeclarator @@ var "id" @@
            (just $ JavaDsl.variableInitializerExpression (var "rhs"))])))


sanitizeJavaName :: TTermDefinition (String -> String)
sanitizeJavaName = def "sanitizeJavaName" $
  lambda "name" $
    Logic.ifElse (isEscaped @@ var "name")
      (unescape @@ var "name")
      (Logic.ifElse (Equality.equal (var "name") (string "_"))
        (string "ignored")
        (Formatting.sanitizeWithUnderscores @@ JavaLanguageSource.reservedWords @@ var "name"))

isEscaped :: TTermDefinition (String -> Bool)
isEscaped = def "isEscaped" $
  lambda "s" $ Equality.equal (Strings.charAt (int32 0) (var "s")) (int32 36)

unescape :: TTermDefinition (String -> String)
unescape = def "unescape" $
  lambda "s" $ Strings.fromList (Lists.tail (Strings.toList (var "s")))


javaPackageDeclaration :: TTermDefinition (Namespace -> Java.PackageDeclaration)
javaPackageDeclaration = def "javaPackageDeclaration" $
  lambda "ns" $ JavaDsl.packageDeclaration
    (list ([] :: [TTerm Java.PackageModifier]))
    (Lists.map (lambda "s" $ JavaDsl.identifier (var "s")) (Strings.splitOn (string ".") (Packaging.unNamespace $ var "ns")))


overrideAnnotation :: TTermDefinition Java.Annotation
overrideAnnotation = def "overrideAnnotation" $
  JavaDsl.annotationMarker
    (JavaDsl.markerAnnotation (javaTypeName @@ JavaDsl.identifier (string "Override")))


methodInvocation :: TTermDefinition (Maybe (Either Java.ExpressionName Java.Primary) -> Java.Identifier -> [Java.Expression] -> Java.MethodInvocation)
methodInvocation = def "methodInvocation" $
  lambda "lhs" $ lambda "methodName" $ lambda "args" $ lets [
    "header">: Maybes.cases (var "lhs")
      (JavaDsl.methodInvocationHeaderSimple (JavaDsl.methodName (var "methodName")))
      (lambda "either" $ JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (Eithers.either_
            (lambda "en" $ JavaDsl.methodInvocationVariantExpression (var "en"))
            (lambda "p" $ JavaDsl.methodInvocationVariantPrimary (var "p"))
            (var "either"))
          (list ([] :: [TTerm Java.TypeArgument]))
          (var "methodName")))] $
    JavaDsl.methodInvocation_ (var "header") (var "args")

methodInvocationStatic :: TTermDefinition (Java.Identifier -> Java.Identifier -> [Java.Expression] -> Java.MethodInvocation)
methodInvocationStatic = def "methodInvocationStatic" $
  lambda "self" $ lambda "methodName" $ lambda "args" $
    methodInvocation @@
      (just $ left (javaIdentifierToJavaExpressionName @@ var "self")) @@
      var "methodName" @@
      var "args"

methodDeclaration :: TTermDefinition ([Java.MethodModifier] -> [Java.TypeParameter] -> [Java.Annotation]
  -> String -> [Java.FormalParameter] -> Java.Result -> Maybe [Java.BlockStatement] -> Java.ClassBodyDeclaration)
methodDeclaration = def "methodDeclaration" $
  lambda "mods" $ lambda "tparams" $ lambda "anns" $ lambda "methodName" $ lambda "params" $ lambda "result" $ lambda "stmts" $
    javaMethodDeclarationToJavaClassBodyDeclaration @@
      (JavaDsl.methodDeclaration_ (var "anns") (var "mods")
        (javaMethodHeader @@ var "tparams" @@ var "methodName" @@ var "params" @@ var "result")
        (javaMethodBody @@ var "stmts"))

interfaceMethodDeclaration :: TTermDefinition ([Java.InterfaceMethodModifier] -> [Java.TypeParameter] -> String
  -> [Java.FormalParameter] -> Java.Result -> Maybe [Java.BlockStatement] -> Java.InterfaceMemberDeclaration)
interfaceMethodDeclaration = def "interfaceMethodDeclaration" $
  lambda "mods" $ lambda "tparams" $ lambda "methodName" $ lambda "params" $ lambda "result" $ lambda "stmts" $
    JavaDsl.interfaceMemberDeclInterfaceMethod
      (JavaDsl.interfaceMethodDeclaration_ (var "mods")
        (javaMethodHeader @@ var "tparams" @@ var "methodName" @@ var "params" @@ var "result")
        (javaMethodBody @@ var "stmts"))


javaEquals :: TTermDefinition (Java.EqualityExpression -> Java.RelationalExpression -> Java.EqualityExpression)
javaEquals = def "javaEquals" $
  lambda "lhs" $ lambda "rhs" $
    JavaDsl.equalityExpressionEqual
      (JavaDsl.equalityExpressionBinary (var "lhs") (var "rhs"))

javaEqualsNull :: TTermDefinition (Java.EqualityExpression -> Java.EqualityExpression)
javaEqualsNull = def "javaEqualsNull" $
  lambda "lhs" $ javaEquals @@ var "lhs" @@
    (javaLiteralToJavaRelationalExpression @@ JavaDsl.literalNull)

javaInstanceOf :: TTermDefinition (Java.RelationalExpression -> Java.ReferenceType -> Java.RelationalExpression)
javaInstanceOf = def "javaInstanceOf" $
  lambda "lhs" $ lambda "rhs" $
    JavaDsl.relationalExpressionInstanceOf
      (JavaDsl.relationalExpressionInstanceOf_ (var "lhs") (var "rhs"))


javaThrowIllegalArgumentException :: TTermDefinition ([Java.Expression] -> Java.Statement)
javaThrowIllegalArgumentException = def "javaThrowIllegalArgumentException" $
  lambda "args" $ javaThrowStatement @@
    (javaConstructorCall @@
      (javaConstructorName @@ JavaDsl.identifier (string "IllegalArgumentException") @@ nothing) @@
      var "args" @@ nothing)

javaThrowIllegalStateException :: TTermDefinition ([Java.Expression] -> Java.Statement)
javaThrowIllegalStateException = def "javaThrowIllegalStateException" $
  lambda "args" $ javaThrowStatement @@
    (javaConstructorCall @@
      (javaConstructorName @@ JavaDsl.identifier (string "IllegalStateException") @@ nothing) @@
      var "args" @@ nothing)


addExpressions :: TTermDefinition ([Java.MultiplicativeExpression] -> Java.AdditiveExpression)
addExpressions = def "addExpressions" $
  lambda "exprs" $ lets [
    "first">: JavaDsl.additiveExpressionUnary (Lists.head (var "exprs")),
    "rest">: Lists.tail (var "exprs")] $
    Lists.foldl
      (lambda "ae" $ lambda "me" $
        JavaDsl.additiveExpressionPlus (JavaDsl.additiveExpressionBinary (var "ae") (var "me")))
      (var "first")
      (var "rest")


-- | Compute the qualified Java name (TypeIdentifier, ClassTypeQualifier) for a Hydra name.
--   If qualify is True, the namespace is converted to a package qualifier.
--   mlocal is an optional local suffix (for inner class names).
nameToQualifiedJavaName :: TTermDefinition (JavaHelpers.Aliases -> Bool -> Name -> Maybe String
  -> (Java.TypeIdentifier, Java.ClassTypeQualifier))
nameToQualifiedJavaName = def "nameToQualifiedJavaName" $
  lambda "aliases" $ lambda "qualify" $ lambda "name" $ lambda "mlocal" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Packaging.qualifiedNameNamespace (var "qn"),
    "local">: Packaging.qualifiedNameLocal (var "qn"),
    -- Build the alias: if ns is Just, look up in packages map; if not found, create from namespace
    "alias">: Maybes.cases (var "ns_")
      nothing
      (lambda "n" $
        just (Maybes.cases
          (Maps.lookup (var "n")
            (project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases"))
          (JavaNamesSource.javaPackageName @@ (Strings.splitOn (string ".") (unwrap _Namespace @@ var "n")))
          (lambda "id" $ var "id"))),
    -- Build the package qualifier
    "pkg">: Logic.ifElse (var "qualify")
      (Maybes.cases (var "alias") JavaDsl.classTypeQualifierNone
        (lambda "p" $ JavaDsl.classTypeQualifierPackage (var "p")))
      JavaDsl.classTypeQualifierNone,
    -- Build the type identifier
    "jid">: javaTypeIdentifier @@ (Maybes.cases (var "mlocal")
      (sanitizeJavaName @@ var "local")
      (lambda "l" $ (sanitizeJavaName @@ var "local") ++ string "." ++ (sanitizeJavaName @@ var "l")))] $
    pair (var "jid") (var "pkg")

-- | Build a Java ClassType from a Hydra name, with type arguments and optional inner class suffix
nameToJavaClassType :: TTermDefinition (JavaHelpers.Aliases -> Bool -> [Java.TypeArgument] -> Name -> Maybe String -> Java.ClassType)
nameToJavaClassType = def "nameToJavaClassType" $
  lambda "aliases" $ lambda "qualify" $ lambda "args" $ lambda "name" $ lambda "mlocal" $ lets [
    "result">: nameToQualifiedJavaName @@ var "aliases" @@ var "qualify" @@ var "name" @@ var "mlocal",
    "id">: Pairs.first (var "result"),
    "pkg">: Pairs.second (var "result")] $
    JavaDsl.classType (list ([] :: [TTerm Java.Annotation])) (var "pkg") (var "id") (var "args")

-- | Build a Java ReferenceType from a Hydra name
nameToJavaReferenceType :: TTermDefinition (JavaHelpers.Aliases -> Bool -> [Java.TypeArgument] -> Name -> Maybe String -> Java.ReferenceType)
nameToJavaReferenceType = def "nameToJavaReferenceType" $
  lambda "aliases" $ lambda "qualify" $ lambda "args" $ lambda "name" $ lambda "mlocal" $
    JavaDsl.referenceTypeClassOrInterface
      (JavaDsl.classOrInterfaceTypeClass
        (nameToJavaClassType @@ var "aliases" @@ var "qualify" @@ var "args" @@ var "name" @@ var "mlocal"))

-- | Build a Java Identifier from a Hydra name, using the Aliases for package resolution
nameToJavaName :: TTermDefinition (JavaHelpers.Aliases -> Name -> Java.Identifier)
nameToJavaName = def "nameToJavaName" $
  lambda "aliases" $ lambda "name" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Packaging.qualifiedNameNamespace (var "qn"),
    "local">: Packaging.qualifiedNameLocal (var "qn")] $
    Logic.ifElse (isEscaped @@ (Core.unName $ var "name"))
      (JavaDsl.identifier (sanitizeJavaName @@ var "local"))
      (Maybes.cases (var "ns_")
        (JavaDsl.identifier (var "local"))
        (lambda "gname" $ lets [
          "parts">: Maybes.cases
            (Maps.lookup (var "gname")
              (project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases"))
            (Strings.splitOn (string ".") (unwrap _Namespace @@ var "gname"))
            (lambda "pkgName" $
              Lists.map (lambda "i" $ unwrap Java._Identifier @@ var "i")
                (unwrap Java._PackageName @@ var "pkgName")),
          "allParts">: Lists.concat2 (var "parts") (list [sanitizeJavaName @@ var "local"])] $
          JavaDsl.identifier (Strings.intercalate (string ".") (var "allParts"))))

-- | Get the Java TypeIdentifier for a Hydra name
nameToJavaTypeIdentifier :: TTermDefinition (JavaHelpers.Aliases -> Bool -> Name -> Java.TypeIdentifier)
nameToJavaTypeIdentifier = def "nameToJavaTypeIdentifier" $
  lambda "aliases" $ lambda "qualify" $ lambda "name" $
    Pairs.first (nameToQualifiedJavaName @@ var "aliases" @@ var "qualify" @@ var "name" @@ nothing)

-- | Build a Java Type from a type name (for use as formal parameter types)
javaTypeFromTypeName :: TTermDefinition (JavaHelpers.Aliases -> Name -> Java.Type)
javaTypeFromTypeName = def "javaTypeFromTypeName" $
  lambda "aliases" $ lambda "elName" $
    javaTypeVariableToType @@
      (JavaDsl.typeVariable
        (list ([] :: [TTerm Java.Annotation]))
        (nameToJavaTypeIdentifier @@ var "aliases" @@ boolean False @@ var "elName"))


-- | Create a double cast expression: first cast to raw type, then to target type
javaDoubleCastExpression :: TTermDefinition (Java.ReferenceType -> Java.ReferenceType -> Java.UnaryExpression -> Java.CastExpression)
javaDoubleCastExpression = def "javaDoubleCastExpression" $
  lambda "rawRt" $ lambda "targetRt" $ lambda "expr" $ lets [
    "firstCast">: javaCastExpressionToJavaExpression @@ (javaCastExpression @@ var "rawRt" @@ var "expr")] $
    javaCastExpression @@ var "targetRt" @@ (javaExpressionToJavaUnaryExpression @@ var "firstCast")

-- | Create a double cast expression and convert to Java expression
javaDoubleCastExpressionToJavaExpression :: TTermDefinition (Java.ReferenceType -> Java.ReferenceType -> Java.UnaryExpression -> Java.Expression)
javaDoubleCastExpressionToJavaExpression = def "javaDoubleCastExpressionToJavaExpression" $
  lambda "rawRt" $ lambda "targetRt" $ lambda "expr" $
    javaCastExpressionToJavaExpression @@ (javaDoubleCastExpression @@ var "rawRt" @@ var "targetRt" @@ var "expr")


-- | Java byte primitive type
javaBytePrimitiveType :: TTermDefinition Java.PrimitiveTypeWithAnnotations
javaBytePrimitiveType = def "javaBytePrimitiveType" $
  JavaDsl.primitiveTypeWithAnnotations
    (JavaDsl.primitiveTypeNumeric (JavaDsl.numericTypeIntegral JavaDsl.integralTypeByte))
    (list ([] :: [TTerm Java.Annotation]))


-- | The reference type for the visitor return type variable "r"
visitorTypeVariable :: TTermDefinition Java.ReferenceType
visitorTypeVariable = def "visitorTypeVariable" $
  javaTypeVariable @@ string "r"


-- | Look up the Java variable name for a Hydra variable, applying any renames
lookupJavaVarName :: TTermDefinition (JavaHelpers.Aliases -> Name -> Name)
lookupJavaVarName = def "lookupJavaVarName" $
  lambda "aliases" $ lambda "name" $
    Maybes.cases
      (Maps.lookup (var "name")
        (project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases"))
      (var "name")
      (lambda "renamed" $ var "renamed")


-- | Compute the class name for a union variant
variantClassName :: TTermDefinition (Bool -> Name -> Name -> Name)
variantClassName = def "variantClassName" $
  lambda "qualify" $ lambda "elName" $ lambda "fname" $ lets [
    "qn">: Names.qualifyName @@ var "elName",
    "ns_">: Packaging.qualifiedNameNamespace (var "qn"),
    "local">: Packaging.qualifiedNameLocal (var "qn"),
    "flocal">: Formatting.capitalize @@ (Core.unName $ var "fname"),
    "local1">: Logic.ifElse (var "qualify")
      (Strings.cat2 (Strings.cat2 (var "local") (string ".")) (var "flocal"))
      (Logic.ifElse (Equality.equal (var "flocal") (var "local"))
        (Strings.cat2 (var "flocal") (string "_"))
        (var "flocal"))] $
    Names.unqualifyName @@ Packaging.qualifiedName (var "ns_") (var "local1")


-- | Create a variable declaration statement with an explicit type
variableDeclarationStatement :: TTermDefinition (JavaHelpers.Aliases -> Java.Type -> Java.Identifier -> Java.Expression -> Java.BlockStatement)
variableDeclarationStatement = def "variableDeclarationStatement" $
  lambda "aliases" $ lambda "jtype" $ lambda "id" $ lambda "rhs" $ lets [
    "init_">: JavaDsl.variableInitializerExpression (var "rhs"),
    "vdec">: javaVariableDeclarator @@ var "id" @@ just (var "init_")] $
    JavaDsl.blockStatementLocalVariableDeclaration
      (JavaDsl.localVariableDeclarationStatement
        (JavaDsl.localVariableDeclaration
          (list ([] :: [TTerm Java.VariableModifier]))
          (JavaDsl.localVariableTypeType (JavaDsl.unannType (var "jtype")))
          (list [var "vdec"])))


-- | Convert a string to a multiplicative expression (for string concatenation)
javaStringMultiplicativeExpression :: TTermDefinition (String -> Java.MultiplicativeExpression)
javaStringMultiplicativeExpression = def "javaStringMultiplicativeExpression" $
  lambda "s" $ javaLiteralToJavaMultiplicativeExpression @@ (javaString @@ var "s")


-- | The @SuppressWarnings("unchecked") annotation
suppressWarningsUncheckedAnnotation :: TTermDefinition Java.Annotation
suppressWarningsUncheckedAnnotation = def "suppressWarningsUncheckedAnnotation" $
  inject Java._Annotation Java._Annotation_singleElement
    (record Java._SingleElementAnnotation [
      Java._SingleElementAnnotation_name>>: javaTypeName @@ JavaDsl.identifier (string "SuppressWarnings"),
      Java._SingleElementAnnotation_value>>:
        just (JavaDsl.elementValueConditional
          (JavaDsl.conditionalExpressionSimple
            (JavaDsl.conditionalOrExpression
              (list [wrap Java._ConditionalAndExpression
                (list [javaPostfixExpressionToJavaInclusiveOrExpression @@
                  (JavaDsl.postfixExpressionPrimary
                    (javaLiteralToJavaPrimary @@ (javaString @@ string "unchecked")))])]))))])


-- | Create a static method invocation with explicit type arguments
methodInvocationStaticWithTypeArgs :: TTermDefinition (Java.Identifier -> Java.Identifier -> [Java.TypeArgument] -> [Java.Expression] -> Java.MethodInvocation)
methodInvocationStaticWithTypeArgs = def "methodInvocationStaticWithTypeArgs" $
  lambda "self" $ lambda "methodName" $ lambda "targs" $ lambda "args" $ lets [
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantExpression
          (javaIdentifierToJavaExpressionName @@ var "self"))
        (var "targs")
        (var "methodName"))] $
    JavaDsl.methodInvocation_ (var "header") (var "args")


-- | Create an array creation expression for primitive byte arrays with an initializer
javaArrayCreation :: TTermDefinition (Java.PrimitiveTypeWithAnnotations -> Maybe Java.ArrayInitializer -> Java.Expression)
javaArrayCreation = def "javaArrayCreation" $
  lambda "primType" $ lambda "minit" $ lets [
    "init_">: Maybes.cases (var "minit")
      (JavaDsl.arrayInitializer (list ([] :: [TTerm Java.VariableInitializer])))
      (lambda "i" $ var "i")] $
    javaPrimaryToJavaExpression @@
      (JavaDsl.primaryArrayCreation
        (JavaDsl.arrayCreationExpressionPrimitiveArray
          (record Java._ArrayCreationExpression_PrimitiveArray [
            Java._ArrayCreationExpression_PrimitiveArray_type>>: var "primType",
            Java._ArrayCreationExpression_PrimitiveArray_dims>>:
              list ([] :: [TTerm Java.Dims]),
            Java._ArrayCreationExpression_PrimitiveArray_array>>: var "init_"])))

-- | Create an array initializer from a list of expressions
javaArrayInitializer :: TTermDefinition ([Java.Expression] -> Java.ArrayInitializer)
javaArrayInitializer = def "javaArrayInitializer" $
  lambda "exprs" $ wrap Java._ArrayInitializer
    (list [Lists.map (lambda "e" $ JavaDsl.variableInitializerExpression (var "e")) (var "exprs")])


-- | Create an assignment statement that assigns a field name to 'this.fieldName'
toAssignStmt :: TTermDefinition (Name -> Java.Statement)
toAssignStmt = def "toAssignStmt" $
  lambda "fname" $ lets [
    "id">: fieldNameToJavaIdentifier @@ var "fname",
    "lhs">: inject Java._LeftHandSide Java._LeftHandSide_fieldAccess
      (JavaDsl.fieldAccess
        (JavaDsl.fieldAccessQualifierPrimary
          (JavaDsl.primaryNoNewArray JavaDsl.primaryThis))
        (var "id")),
    "rhs">: fieldNameToJavaExpression @@ var "fname"] $
    javaAssignmentStatement @@ var "lhs" @@ var "rhs"


-- | Extract the string name from a TypeParameter
unTypeParameter :: TTermDefinition (Java.TypeParameter -> String)
unTypeParameter = def "unTypeParameter" $
  lambda "tp" $
    JavaDsl.unIdentifier (JavaDsl.unTypeIdentifier (JavaDsl.typeParameterIdentifier (var "tp")))


-- | Create an initial Aliases for a module with all fields set to empty/default
importAliasesForModule :: TTermDefinition (Module -> JavaHelpers.Aliases)
importAliasesForModule = def "importAliasesForModule" $
  lambda "mod" $
    record JavaHelpers._Aliases [
      JavaHelpers._Aliases_currentNamespace>>: Packaging.moduleNamespace (var "mod"),
      JavaHelpers._Aliases_packages>>: (Maps.empty :: TTerm (M.Map Namespace Java.PackageName)),
      JavaHelpers._Aliases_branchVars>>: (Sets.empty :: TTerm (S.Set Name)),
      JavaHelpers._Aliases_recursiveVars>>: (Sets.empty :: TTerm (S.Set Name)),
      JavaHelpers._Aliases_inScopeTypeParams>>: (Sets.empty :: TTerm (S.Set Name)),
      JavaHelpers._Aliases_polymorphicLocals>>: (Sets.empty :: TTerm (S.Set Name)),
      JavaHelpers._Aliases_inScopeJavaVars>>: (Sets.empty :: TTerm (S.Set Name)),
      JavaHelpers._Aliases_varRenames>>: (Maps.empty :: TTerm (M.Map Name Name)),
      JavaHelpers._Aliases_lambdaVars>>: (Sets.empty :: TTerm (S.Set Name)),
      JavaHelpers._Aliases_typeVarSubst>>: (Maps.empty :: TTerm (M.Map Name Name)),
      JavaHelpers._Aliases_trustedTypeVars>>: (Sets.empty :: TTerm (S.Set Name)),
      JavaHelpers._Aliases_methodCodomain>>: (nothing :: TTerm (Maybe Type)),
      JavaHelpers._Aliases_thunkedVars>>: (Sets.empty :: TTerm (S.Set Name))]


-- | Build a Java class declaration with optional superclass
javaClassDeclaration :: TTermDefinition (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [Java.ClassModifier]
  -> Maybe Name -> [Java.InterfaceType] -> [Java.ClassBodyDeclarationWithComments] -> Java.ClassDeclaration)
javaClassDeclaration = def "javaClassDeclaration" $
  lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "mods" $
    lambda "supname" $ lambda "impls" $ lambda "bodyDecls" $ lets [
    "extends_">: Maybes.map
      (lambda "n" $ nameToJavaClassType @@ var "aliases" @@ boolean True @@ list ([] :: [TTerm Java.TypeArgument]) @@ var "n" @@ nothing)
      (var "supname")] $
    inject Java._ClassDeclaration Java._ClassDeclaration_normal
      (record Java._NormalClassDeclaration [
        Java._NormalClassDeclaration_modifiers>>: var "mods",
        Java._NormalClassDeclaration_identifier>>: javaDeclName @@ var "elName",
        Java._NormalClassDeclaration_parameters>>: var "tparams",
        Java._NormalClassDeclaration_extends>>: var "extends_",
        Java._NormalClassDeclaration_implements>>: var "impls",
        Java._NormalClassDeclaration_body>>: JavaDsl.classBody (var "bodyDecls")])


-- | Build a Java constructor declaration
makeConstructor :: TTermDefinition (JavaHelpers.Aliases -> Name -> Bool -> [Java.FormalParameter]
  -> [Java.BlockStatement] -> Java.ClassBodyDeclaration)
makeConstructor = def "makeConstructor" $
  lambda "aliases" $ lambda "elName" $ lambda "private" $ lambda "params" $ lambda "stmts" $ lets [
    "nm">: JavaDsl.simpleTypeName (nameToJavaTypeIdentifier @@ var "aliases" @@ boolean False @@ var "elName"),
    "cons">: JavaDsl.constructorDeclarator (list ([] :: [TTerm Java.TypeParameter])) (var "nm") nothing (var "params"),
    "mods">: list [Logic.ifElse (var "private")
      (inject Java._ConstructorModifier Java._ConstructorModifier_private unit)
      (inject Java._ConstructorModifier Java._ConstructorModifier_public unit)],
    "body">: JavaDsl.constructorBody nothing (var "stmts")] $
    inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_constructorDeclaration
      (JavaDsl.constructorDeclaration (var "mods") (var "cons") nothing (var "body"))


-- | Build the accept method for the visitor pattern
toAcceptMethod :: TTermDefinition (Bool -> [Java.TypeParameter] -> Java.ClassBodyDeclaration)
toAcceptMethod = def "toAcceptMethod" $
  lambda "abstract" $ lambda "vtparams" $ lets [
    "mods">: Logic.ifElse (var "abstract")
      (list [inject Java._MethodModifier Java._MethodModifier_public unit,
             inject Java._MethodModifier Java._MethodModifier_abstract unit])
      (list [inject Java._MethodModifier Java._MethodModifier_public unit]),
    "tparams">: list [javaTypeParameter @@ asTerm JavaNamesSource.visitorReturnParameter],
    "anns">: Logic.ifElse (var "abstract")
      (list ([] :: [TTerm Java.Annotation]))
      (list [asTerm overrideAnnotation]),
    "typeArgs">: Lists.map
      (lambda "tp" $ JavaDsl.typeArgumentReference (typeParameterToReferenceType @@ var "tp"))
      (var "vtparams"),
    "ref">: javaClassTypeToJavaType @@
      (JavaDsl.classType
        (list ([] :: [TTerm Java.Annotation]))
        JavaDsl.classTypeQualifierNone
        (javaTypeIdentifier @@ asTerm JavaNamesSource.visitorName)
        (Lists.concat2 (var "typeArgs") (list [JavaDsl.typeArgumentReference (asTerm visitorTypeVariable)]))),
    "param">: javaTypeToJavaFormalParameter @@ var "ref" @@ wrap _Name (string "visitor"),
    "result">: javaTypeToJavaResult @@ JavaDsl.typeReference (asTerm visitorTypeVariable),
    "returnExpr">: javaMethodInvocationToJavaExpression @@
      (methodInvocationStatic @@ JavaDsl.identifier (string "visitor") @@ JavaDsl.identifier (asTerm JavaNamesSource.visitMethodName)
        @@ list [asTerm javaThis]),
    "body">: Logic.ifElse (var "abstract")
      (nothing :: TTerm (Maybe [Java.BlockStatement]))
      (just (list [JavaDsl.blockStatementStatement (javaReturnStatement @@ just (var "returnExpr"))]))] $
    methodDeclaration @@ var "mods" @@ var "tparams" @@ var "anns"
      @@ asTerm JavaNamesSource.acceptMethodName @@ list [var "param"] @@ var "result" @@ var "body"


-- | Convert a Java Type to an array type
toJavaArrayType :: TTermDefinition (Java.Type -> Context -> Either (InContext Error) Java.Type)
toJavaArrayType = def "toJavaArrayType" $
  lambda "t" $ "cx" ~>
  cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt" $
      cases Java._ReferenceType (var "rt") Nothing [
        Java._ReferenceType_classOrInterface>>: lambda "cit" $
          right (JavaDsl.typeReference (JavaDsl.referenceTypeArray
            (JavaDsl.arrayType
              (JavaDsl.dims (list [list ([] :: [TTerm Java.Annotation])]))
              (inject Java._ArrayType_Variant Java._ArrayType_Variant_classOrInterface (var "cit"))))),
        Java._ReferenceType_array>>: lambda "at" $ lets [
          "oldDims">: unwrap Java._Dims @@ (project Java._ArrayType Java._ArrayType_dims @@ var "at"),
          "newDims">: JavaDsl.dims (Lists.concat2 (var "oldDims") (list [list ([] :: [TTerm Java.Annotation])])),
          "variant">: project Java._ArrayType Java._ArrayType_variant @@ var "at"] $
          right (JavaDsl.typeReference (JavaDsl.referenceTypeArray
            (JavaDsl.arrayType (var "newDims") (var "variant")))),
        Java._ReferenceType_variable>>: constant $
          Ctx.failInContext (Error.errorOther $ Error.otherError $ string "don't know how to make Java reference type into array type") (var "cx")],
    Java._Type_primitive>>: constant $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "don't know how to make Java type into array type") (var "cx")]


-- | Extract the reference type from a Java type, failing if it's a primitive type
javaTypeToJavaReferenceType :: TTermDefinition (Java.Type -> Context -> Either (InContext Error) Java.ReferenceType)
javaTypeToJavaReferenceType = def "javaTypeToJavaReferenceType" $
  lambda "t" $ "cx" ~>
  cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt" $ right (var "rt"),
    Java._Type_primitive>>: constant $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "expected a Java reference type") (var "cx")]


-- | Extract the raw type (without type arguments) from a reference type
javaReferenceTypeToRawType :: TTermDefinition (Java.ReferenceType -> Java.ReferenceType)
javaReferenceTypeToRawType = def "javaReferenceTypeToRawType" $
  lambda "rt" $ cases Java._ReferenceType (var "rt")
    (Just $ var "rt") [
    Java._ReferenceType_classOrInterface>>: lambda "cit" $
      cases Java._ClassOrInterfaceType (var "cit") Nothing [
        Java._ClassOrInterfaceType_class>>: lambda "ct" $ lets [
          "anns">: project Java._ClassType Java._ClassType_annotations @@ var "ct",
          "qual">: project Java._ClassType Java._ClassType_qualifier @@ var "ct",
          "id">: project Java._ClassType Java._ClassType_identifier @@ var "ct"] $
          JavaDsl.referenceTypeClassOrInterface
            (JavaDsl.classOrInterfaceTypeClass
              (JavaDsl.classType (var "anns") (var "qual") (var "id")
                (list ([] :: [TTerm Java.TypeArgument])))),
        Java._ClassOrInterfaceType_interface>>: lambda "it" $ lets [
          "ct">: unwrap Java._InterfaceType @@ var "it",
          "anns">: project Java._ClassType Java._ClassType_annotations @@ var "ct",
          "qual">: project Java._ClassType Java._ClassType_qualifier @@ var "ct",
          "id">: project Java._ClassType Java._ClassType_identifier @@ var "ct"] $
          JavaDsl.referenceTypeClassOrInterface
            (inject Java._ClassOrInterfaceType Java._ClassOrInterfaceType_interface
              (JavaDsl.interfaceType
                (JavaDsl.classType (var "anns") (var "qual") (var "id")
                  (list ([] :: [TTerm Java.TypeArgument])))))]]


-- | Add a reference type as a type argument to an existing Java type
addJavaTypeParameter :: TTermDefinition (Java.ReferenceType -> Java.Type -> Context -> Either (InContext Error) Java.Type)
addJavaTypeParameter = def "addJavaTypeParameter" $
  lambda "rt" $ lambda "t" $ "cx" ~>
  cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt1" $
      cases Java._ReferenceType (var "rt1") Nothing [
        Java._ReferenceType_classOrInterface>>: lambda "cit" $
          cases Java._ClassOrInterfaceType (var "cit") Nothing [
            Java._ClassOrInterfaceType_class>>: lambda "ct" $ lets [
              "anns">: project Java._ClassType Java._ClassType_annotations @@ var "ct",
              "qual">: project Java._ClassType Java._ClassType_qualifier @@ var "ct",
              "id">: project Java._ClassType Java._ClassType_identifier @@ var "ct",
              "args">: project Java._ClassType Java._ClassType_arguments @@ var "ct"] $
              right (JavaDsl.typeReference
                (JavaDsl.referenceTypeClassOrInterface
                  (JavaDsl.classOrInterfaceTypeClass
                    (JavaDsl.classType (var "anns") (var "qual") (var "id")
                      (Lists.concat2 (var "args") (list [JavaDsl.typeArgumentReference (var "rt")])))))),
            Java._ClassOrInterfaceType_interface>>: constant $
              Ctx.failInContext (Error.errorOther $ Error.otherError $ string "expected a Java class type") (var "cx")],
        Java._ReferenceType_variable>>: lambda "tv" $
          right (javaTypeVariableToType @@ var "tv"),
        Java._ReferenceType_array>>: constant $
          Ctx.failInContext (Error.errorOther $ Error.otherError $ string "expected a Java class or interface type, or a variable") (var "cx")],
    Java._Type_primitive>>: constant $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "expected a reference type") (var "cx")]


-- | Generate a unique variable name that doesn't conflict with in-scope names
uniqueVarName :: TTermDefinition (JavaHelpers.Aliases -> Name -> Name)
uniqueVarName = def "uniqueVarName" $
  lambda "aliases" $ lambda "name" $
    Logic.ifElse
      (Sets.member (var "name")
        (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases"))
      (uniqueVarName_go @@ var "aliases" @@ Core.unName (var "name") @@ int32 2)
      (var "name")

-- | Helper for uniqueVarName
uniqueVarName_go :: TTermDefinition (JavaHelpers.Aliases -> String -> Int -> Name)
uniqueVarName_go = def "uniqueVarName_go" $
  lambda "aliases" $ lambda "base" $ lambda "n" $ lets [
    "candidate">: wrap _Name (Strings.cat2 (var "base") (Literals.showInt32 (var "n")))] $
    Logic.ifElse
      (Sets.member (var "candidate")
        (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases"))
      (uniqueVarName_go @@ var "aliases" @@ var "base" @@ Math.add (var "n") (int32 1))
      (var "candidate")

-- | Add a variable to the in-scope set and return updated aliases
addInScopeVar :: TTermDefinition (Name -> JavaHelpers.Aliases -> JavaHelpers.Aliases)
addInScopeVar = def "addInScopeVar" $
  lambda "name" $ lambda "aliases" $
    record JavaHelpers._Aliases [
      JavaHelpers._Aliases_currentNamespace>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_currentNamespace @@ var "aliases",
      JavaHelpers._Aliases_packages>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases",
      JavaHelpers._Aliases_branchVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases",
      JavaHelpers._Aliases_recursiveVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases",
      JavaHelpers._Aliases_inScopeTypeParams>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases",
      JavaHelpers._Aliases_polymorphicLocals>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_polymorphicLocals @@ var "aliases",
      JavaHelpers._Aliases_inScopeJavaVars>>:
        Sets.insert (var "name") (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases"),
      JavaHelpers._Aliases_varRenames>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases",
      JavaHelpers._Aliases_lambdaVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases",
      JavaHelpers._Aliases_typeVarSubst>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases",
      JavaHelpers._Aliases_trustedTypeVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases",
      JavaHelpers._Aliases_methodCodomain>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_methodCodomain @@ var "aliases",
      JavaHelpers._Aliases_thunkedVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases"]

-- | Add multiple variables to the in-scope set
addInScopeVars :: TTermDefinition ([Name] -> JavaHelpers.Aliases -> JavaHelpers.Aliases)
addInScopeVars = def "addInScopeVars" $
  lambda "names" $ lambda "aliases" $
    Lists.foldl (lambda "a" $ lambda "n" $ addInScopeVar @@ var "n" @@ var "a")
      (var "aliases") (var "names")

-- | Register a variable rename
addVarRename :: TTermDefinition (Name -> Name -> JavaHelpers.Aliases -> JavaHelpers.Aliases)
addVarRename = def "addVarRename" $
  lambda "original" $ lambda "renamed" $ lambda "aliases" $
    record JavaHelpers._Aliases [
      JavaHelpers._Aliases_currentNamespace>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_currentNamespace @@ var "aliases",
      JavaHelpers._Aliases_packages>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases",
      JavaHelpers._Aliases_branchVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases",
      JavaHelpers._Aliases_recursiveVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases",
      JavaHelpers._Aliases_inScopeTypeParams>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases",
      JavaHelpers._Aliases_polymorphicLocals>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_polymorphicLocals @@ var "aliases",
      JavaHelpers._Aliases_inScopeJavaVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases",
      JavaHelpers._Aliases_varRenames>>:
        Maps.insert (var "original") (var "renamed")
          (project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases"),
      JavaHelpers._Aliases_lambdaVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases",
      JavaHelpers._Aliases_typeVarSubst>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases",
      JavaHelpers._Aliases_trustedTypeVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases",
      JavaHelpers._Aliases_methodCodomain>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_methodCodomain @@ var "aliases",
      JavaHelpers._Aliases_thunkedVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases"]
