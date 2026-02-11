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
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S

-- Additional imports
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Ext.Java.Helpers as JavaHelpers
import qualified Hydra.Ext.Dsl.Java.Syntax as JavaDsl
import qualified Hydra.Ext.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Ext.Sources.Java.Helpers as JavaHelpersSource
import qualified Hydra.Ext.Sources.Java.Language as JavaLanguageSource
import qualified Hydra.Ext.Sources.Java.Names as JavaNamesSource
import qualified Hydra.Ext.Sources.Java.Serde as JavaSerdeSource


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.java.utils"

module_ :: Module
module_ = Module ns elements
    [moduleNamespace JavaLanguageSource.module_, JavaNamesSource.ns, JavaSerdeSource.ns, Formatting.ns, Names.ns, Serialization.ns]
    (JavaHelpersSource.ns:JavaSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Java utilities for constructing Java syntax trees"
  where
    elements = [
      -- Identifier/Name constructors
      toBinding javaIdentifier,
      toBinding javaTypeIdentifier,
      toBinding javaTypeName,
      toBinding javaDeclName,
      toBinding javaVariableName,
      toBinding javaVariableDeclaratorId,
      toBinding javaVariableDeclarator,
      -- Literal constructors
      toBinding javaBoolean,
      toBinding javaInt,
      toBinding javaString,
      -- Primary/Expression chain
      toBinding javaLiteralToJavaPrimary,
      toBinding javaExpressionToJavaPrimary,
      toBinding javaPrimaryToJavaUnaryExpression,
      toBinding javaPrimaryToJavaExpression,
      toBinding javaPostfixExpressionToJavaUnaryExpression,
      toBinding javaPostfixExpressionToJavaExpression,
      toBinding javaPostfixExpressionToJavaRelationalExpression,
      toBinding javaUnaryExpressionToJavaRelationalExpression,
      toBinding javaUnaryExpressionToJavaExpression,
      toBinding javaRelationalExpressionToJavaExpression,
      toBinding javaRelationalExpressionToJavaUnaryExpression,
      toBinding javaMultiplicativeExpressionToJavaRelationalExpression,
      toBinding javaLiteralToJavaMultiplicativeExpression,
      toBinding javaLiteralToJavaRelationalExpression,
      toBinding javaLiteralToJavaExpression,
      toBinding javaIdentifierToJavaExpressionName,
      toBinding javaIdentifierToJavaExpression,
      toBinding javaIdentifierToJavaRelationalExpression,
      toBinding javaIdentifierToJavaUnaryExpression,
      toBinding javaExpressionNameToJavaExpression,
      toBinding javaFieldAccessToJavaExpression,
      toBinding javaCastExpressionToJavaExpression,
      toBinding javaMethodInvocationToJavaPrimary,
      toBinding javaMethodInvocationToJavaExpression,
      toBinding javaMethodInvocationToJavaPostfixExpression,
      toBinding javaMethodInvocationToJavaStatement,
      toBinding javaConditionalAndExpressionToJavaExpression,
      toBinding javaEqualityExpressionToJavaInclusiveOrExpression,
      toBinding javaEqualityExpressionToJavaExpression,
      toBinding javaPostfixExpressionToJavaEqualityExpression,
      toBinding javaPostfixExpressionToJavaInclusiveOrExpression,
      toBinding javaAdditiveExpressionToJavaExpression,
      -- Expression-to-unary
      toBinding javaExpressionToJavaUnaryExpression,
      -- Type constructors
      toBinding javaPrimitiveTypeToJavaType,
      toBinding javaClassTypeToJavaType,
      toBinding javaTypeVariableToType,
      toBinding javaRefType,
      toBinding javaClassType,
      toBinding javaTypeVariable,
      toBinding javaBooleanType,
      toBinding javaIntType,
      -- Literal expressions
      toBinding javaBooleanExpression,
      toBinding javaIntExpression,
      -- Cast expressions
      toBinding javaCastExpression,
      toBinding javaCastPrimitive,
      -- Statement constructors
      toBinding javaReturnStatement,
      toBinding javaThrowStatement,
      toBinding javaEmptyStatement,
      toBinding javaAssignmentStatement,
      toBinding javaStatementsToBlock,
      -- Lambda constructors
      toBinding javaLambda,
      toBinding javaLambdaFromBlock,
      -- Method/Constructor helpers
      toBinding javaMethodBody,
      toBinding javaMethodHeader,
      toBinding javaMethodDeclarationToJavaClassBodyDeclaration,
      toBinding javaInterfaceDeclarationToJavaClassBodyDeclaration,
      toBinding javaMemberField,
      toBinding javaTypeToJavaFormalParameter,
      toBinding javaTypeToJavaResult,
      toBinding javaTypeToJavaTypeArgument,
      toBinding referenceTypeToResult,
      -- Constructor call
      toBinding javaConstructorName,
      toBinding javaConstructorCall,
      -- This
      toBinding javaThis,
      -- Type helpers
      toBinding javaTypeParameter,
      toBinding javaTypeIdentifierToJavaTypeArgument,
      toBinding typeParameterToTypeArgument,
      toBinding typeParameterToReferenceType,
      -- Field helpers
      toBinding fieldNameToJavaIdentifier,
      toBinding fieldNameToJavaExpression,
      toBinding fieldNameToJavaVariableDeclaratorId,
      toBinding fieldNameToJavaVariableDeclarator,
      toBinding fieldExpression,
      -- Variable helpers
      toBinding variableToJavaIdentifier,
      toBinding varDeclarationStatement,
      -- Sanitization
      toBinding sanitizeJavaName,
      toBinding isEscaped,
      toBinding unescape,
      -- Package
      toBinding javaPackageDeclaration,
      -- Annotations
      toBinding overrideAnnotation,
      -- Method invocations
      toBinding methodInvocation,
      toBinding methodInvocationStatic,
      toBinding methodDeclaration,
      toBinding interfaceMethodDeclaration,
      -- Equals/instanceof
      toBinding javaEquals,
      toBinding javaEqualsNull,
      toBinding javaInstanceOf,
      -- Throw helpers
      toBinding javaThrowIllegalArgumentException,
      toBinding javaThrowIllegalStateException,
      -- Misc
      toBinding addExpressions,
      toBinding javaRelationalExpressionToJavaEqualityExpression,
      -- Name-to-Java mapping
      toBinding nameToQualifiedJavaName,
      toBinding nameToJavaClassType,
      toBinding nameToJavaReferenceType,
      toBinding nameToJavaName,
      toBinding nameToJavaTypeIdentifier,
      -- Type from type name
      toBinding javaTypeFromTypeName,
      -- Cast helpers
      toBinding javaDoubleCastExpression,
      toBinding javaDoubleCastExpressionToJavaExpression,
      -- Primitive type constant
      toBinding javaBytePrimitiveType,
      -- Visitor type variable
      toBinding visitorTypeVariable,
      -- Variable name helpers
      toBinding lookupJavaVarName,
      -- Variant class name
      toBinding variantClassName,
      -- Variable declaration
      toBinding variableDeclarationStatement,
      -- String multiplicative expression
      toBinding javaStringMultiplicativeExpression,
      -- Annotation helpers
      toBinding suppressWarningsUncheckedAnnotation,
      -- Method invocation with type args
      toBinding methodInvocationStaticWithTypeArgs,
      -- Array helpers
      toBinding javaArrayCreation,
      toBinding javaArrayInitializer,
      -- Assignment helpers
      toBinding toAssignStmt,
      -- Type parameter extraction
      toBinding unTypeParameter,
      -- Aliases construction
      toBinding importAliasesForModule,
      -- Class declaration
      toBinding javaClassDeclaration,
      -- Constructor
      toBinding makeConstructor,
      -- Accept method for visitor pattern
      toBinding toAcceptMethod,
      -- Array type conversion
      toBinding toJavaArrayType,
      -- Type to reference type
      toBinding javaTypeToJavaReferenceType,
      -- Reference type to raw type
      toBinding javaReferenceTypeToRawType,
      -- Type parameter add
      toBinding addJavaTypeParameter,
      -- Aliases modifiers
      toBinding uniqueVarName,
      toBinding uniqueVarName_go,
      toBinding addInScopeVar,
      toBinding addInScopeVars,
      toBinding addVarRename]

-- =============================================================================
-- Identifier/Name constructors
-- =============================================================================

javaIdentifier :: TBinding (String -> Java.Identifier)
javaIdentifier = def "javaIdentifier" $
  lambda "s" $ JavaDsl.identifier (sanitizeJavaName @@ var "s")

javaTypeIdentifier :: TBinding (String -> Java.TypeIdentifier)
javaTypeIdentifier = def "javaTypeIdentifier" $
  lambda "s" $ JavaDsl.typeIdentifier (JavaDsl.identifier (var "s"))

javaTypeName :: TBinding (Java.Identifier -> Java.TypeName)
javaTypeName = def "javaTypeName" $
  lambda "id" $ JavaDsl.typeName (JavaDsl.typeIdentifier (var "id")) nothing

javaDeclName :: TBinding (Name -> Java.TypeIdentifier)
javaDeclName = def "javaDeclName" $
  lambda "name" $ JavaDsl.typeIdentifier (javaVariableName @@ var "name")

javaVariableName :: TBinding (Name -> Java.Identifier)
javaVariableName = def "javaVariableName" $
  lambda "name" $ javaIdentifier @@ (Names.localNameOf @@ var "name")

javaVariableDeclaratorId :: TBinding (Java.Identifier -> Java.VariableDeclaratorId)
javaVariableDeclaratorId = def "javaVariableDeclaratorId" $
  lambda "id" $ JavaDsl.variableDeclaratorId (var "id") nothing

javaVariableDeclarator :: TBinding (Java.Identifier -> Maybe Java.VariableInitializer -> Java.VariableDeclarator)
javaVariableDeclarator = def "javaVariableDeclarator" $
  lambda "id" $ lambda "minit" $ JavaDsl.variableDeclarator (javaVariableDeclaratorId @@ var "id") (var "minit")

-- =============================================================================
-- Literal constructors
-- =============================================================================

javaBoolean :: TBinding (Bool -> Java.Literal)
javaBoolean = def "javaBoolean" $
  lambda "b" $ JavaDsl.literalBoolean (var "b")

javaInt :: TBinding (Int -> Java.Literal)
javaInt = def "javaInt" $
  lambda "i" $ JavaDsl.literalInteger (JavaDsl.integerLiteral (var "i"))

javaString :: TBinding (String -> Java.Literal)
javaString = def "javaString" $
  lambda "s" $ JavaDsl.literalString (JavaDsl.stringLiteral (var "s"))

-- =============================================================================
-- Primary/Expression conversion chain
-- =============================================================================

javaLiteralToJavaPrimary :: TBinding (Java.Literal -> Java.Primary)
javaLiteralToJavaPrimary = def "javaLiteralToJavaPrimary" $
  lambda "lit" $ JavaDsl.literalToPrimary (var "lit")

javaExpressionToJavaPrimary :: TBinding (Java.Expression -> Java.Primary)
javaExpressionToJavaPrimary = def "javaExpressionToJavaPrimary" $
  lambda "e" $ JavaDsl.expressionToPrimary (var "e")

javaPrimaryToJavaUnaryExpression :: TBinding (Java.Primary -> Java.UnaryExpression)
javaPrimaryToJavaUnaryExpression = def "javaPrimaryToJavaUnaryExpression" $
  lambda "p" $ JavaDsl.unaryExpressionOther
    (JavaDsl.unaryExpressionNotPlusMinusPostfix
      (JavaDsl.postfixExpressionPrimary (var "p")))

javaPrimaryToJavaExpression :: TBinding (Java.Primary -> Java.Expression)
javaPrimaryToJavaExpression = def "javaPrimaryToJavaExpression" $
  lambda "p" $ JavaDsl.primaryToExpression (var "p")

javaPostfixExpressionToJavaUnaryExpression :: TBinding (Java.PostfixExpression -> Java.UnaryExpression)
javaPostfixExpressionToJavaUnaryExpression = def "javaPostfixExpressionToJavaUnaryExpression" $
  lambda "pe" $ JavaDsl.unaryExpressionOther
    (JavaDsl.unaryExpressionNotPlusMinusPostfix (var "pe"))

javaPostfixExpressionToJavaExpression :: TBinding (Java.PostfixExpression -> Java.Expression)
javaPostfixExpressionToJavaExpression = def "javaPostfixExpressionToJavaExpression" $
  lambda "pe" $ JavaDsl.postfixToExpression (var "pe")

javaPostfixExpressionToJavaRelationalExpression :: TBinding (Java.PostfixExpression -> Java.RelationalExpression)
javaPostfixExpressionToJavaRelationalExpression = def "javaPostfixExpressionToJavaRelationalExpression" $
  lambda "pe" $ JavaDsl.postfixToRelationalExpression (var "pe")

javaUnaryExpressionToJavaRelationalExpression :: TBinding (Java.UnaryExpression -> Java.RelationalExpression)
javaUnaryExpressionToJavaRelationalExpression = def "javaUnaryExpressionToJavaRelationalExpression" $
  lambda "ue" $ JavaDsl.relationalExpressionSimple
    (JavaDsl.shiftExpressionUnary
      (JavaDsl.additiveExpressionUnary
        (JavaDsl.multiplicativeExpressionUnary (var "ue"))))

javaUnaryExpressionToJavaExpression :: TBinding (Java.UnaryExpression -> Java.Expression)
javaUnaryExpressionToJavaExpression = def "javaUnaryExpressionToJavaExpression" $
  lambda "ue" $ JavaDsl.unaryToExpression (var "ue")

javaRelationalExpressionToJavaExpression :: TBinding (Java.RelationalExpression -> Java.Expression)
javaRelationalExpressionToJavaExpression = def "javaRelationalExpressionToJavaExpression" $
  lambda "re" $ javaEqualityExpressionToJavaExpression @@
    (JavaDsl.equalityExpressionUnary (var "re"))

javaRelationalExpressionToJavaEqualityExpression :: TBinding (Java.RelationalExpression -> Java.EqualityExpression)
javaRelationalExpressionToJavaEqualityExpression = def "javaRelationalExpressionToJavaEqualityExpression" $
  lambda "re" $ JavaDsl.equalityExpressionUnary (var "re")

javaRelationalExpressionToJavaUnaryExpression :: TBinding (Java.RelationalExpression -> Java.UnaryExpression)
javaRelationalExpressionToJavaUnaryExpression = def "javaRelationalExpressionToJavaUnaryExpression" $
  lambda "re" $ JavaDsl.relationalToUnary (var "re")

javaMultiplicativeExpressionToJavaRelationalExpression :: TBinding (Java.MultiplicativeExpression -> Java.RelationalExpression)
javaMultiplicativeExpressionToJavaRelationalExpression = def "javaMultiplicativeExpressionToJavaRelationalExpression" $
  lambda "me" $ JavaDsl.relationalExpressionSimple
    (JavaDsl.shiftExpressionUnary
      (JavaDsl.additiveExpressionUnary (var "me")))

javaLiteralToJavaMultiplicativeExpression :: TBinding (Java.Literal -> Java.MultiplicativeExpression)
javaLiteralToJavaMultiplicativeExpression = def "javaLiteralToJavaMultiplicativeExpression" $
  lambda "lit" $ JavaDsl.literalToMultiplicativeExpression (var "lit")

javaLiteralToJavaRelationalExpression :: TBinding (Java.Literal -> Java.RelationalExpression)
javaLiteralToJavaRelationalExpression = def "javaLiteralToJavaRelationalExpression" $
  lambda "lit" $ JavaDsl.literalToRelationalExpression (var "lit")

javaLiteralToJavaExpression :: TBinding (Java.Literal -> Java.Expression)
javaLiteralToJavaExpression = def "javaLiteralToJavaExpression" $
  lambda "lit" $ JavaDsl.literalToExpression (var "lit")

javaIdentifierToJavaExpressionName :: TBinding (Java.Identifier -> Java.ExpressionName)
javaIdentifierToJavaExpressionName = def "javaIdentifierToJavaExpressionName" $
  lambda "id" $ JavaDsl.expressionName nothing (var "id")

javaIdentifierToJavaExpression :: TBinding (Java.Identifier -> Java.Expression)
javaIdentifierToJavaExpression = def "javaIdentifierToJavaExpression" $
  lambda "id" $ JavaDsl.identifierToExpression (var "id")

javaIdentifierToJavaRelationalExpression :: TBinding (Java.Identifier -> Java.RelationalExpression)
javaIdentifierToJavaRelationalExpression = def "javaIdentifierToJavaRelationalExpression" $
  lambda "id" $ JavaDsl.identifierToRelationalExpression (var "id")

javaIdentifierToJavaUnaryExpression :: TBinding (Java.Identifier -> Java.UnaryExpression)
javaIdentifierToJavaUnaryExpression = def "javaIdentifierToJavaUnaryExpression" $
  lambda "id" $ JavaDsl.identifierToUnary (var "id")

javaExpressionNameToJavaExpression :: TBinding (Java.ExpressionName -> Java.Expression)
javaExpressionNameToJavaExpression = def "javaExpressionNameToJavaExpression" $
  lambda "en" $ JavaDsl.expressionNameToExpression (var "en")

javaFieldAccessToJavaExpression :: TBinding (Java.FieldAccess -> Java.Expression)
javaFieldAccessToJavaExpression = def "javaFieldAccessToJavaExpression" $
  lambda "fa" $ JavaDsl.fieldAccessToExpression (var "fa")

javaCastExpressionToJavaExpression :: TBinding (Java.CastExpression -> Java.Expression)
javaCastExpressionToJavaExpression = def "javaCastExpressionToJavaExpression" $
  lambda "ce" $ JavaDsl.castExpressionToExpression (var "ce")

javaMethodInvocationToJavaPrimary :: TBinding (Java.MethodInvocation -> Java.Primary)
javaMethodInvocationToJavaPrimary = def "javaMethodInvocationToJavaPrimary" $
  lambda "mi" $ JavaDsl.methodInvocationToPrimary (var "mi")

javaMethodInvocationToJavaExpression :: TBinding (Java.MethodInvocation -> Java.Expression)
javaMethodInvocationToJavaExpression = def "javaMethodInvocationToJavaExpression" $
  lambda "mi" $ JavaDsl.methodInvocationToExpression (var "mi")

javaMethodInvocationToJavaPostfixExpression :: TBinding (Java.MethodInvocation -> Java.PostfixExpression)
javaMethodInvocationToJavaPostfixExpression = def "javaMethodInvocationToJavaPostfixExpression" $
  lambda "mi" $ JavaDsl.methodInvocationToPostfix (var "mi")

javaMethodInvocationToJavaStatement :: TBinding (Java.MethodInvocation -> Java.Statement)
javaMethodInvocationToJavaStatement = def "javaMethodInvocationToJavaStatement" $
  lambda "mi" $ JavaDsl.methodInvocationToStatement (var "mi")

javaConditionalAndExpressionToJavaExpression :: TBinding (Java.ConditionalAndExpression -> Java.Expression)
javaConditionalAndExpressionToJavaExpression = def "javaConditionalAndExpressionToJavaExpression" $
  lambda "cae" $ JavaDsl.conditionalAndToExpression (var "cae")

javaEqualityExpressionToJavaInclusiveOrExpression :: TBinding (Java.EqualityExpression -> Java.InclusiveOrExpression)
javaEqualityExpressionToJavaInclusiveOrExpression = def "javaEqualityExpressionToJavaInclusiveOrExpression" $
  lambda "ee" $ JavaDsl.equalityToInclusiveOr (var "ee")

javaEqualityExpressionToJavaExpression :: TBinding (Java.EqualityExpression -> Java.Expression)
javaEqualityExpressionToJavaExpression = def "javaEqualityExpressionToJavaExpression" $
  lambda "ee" $ JavaDsl.equalityToExpression (var "ee")

javaPostfixExpressionToJavaEqualityExpression :: TBinding (Java.PostfixExpression -> Java.EqualityExpression)
javaPostfixExpressionToJavaEqualityExpression = def "javaPostfixExpressionToJavaEqualityExpression" $
  lambda "pe" $ JavaDsl.equalityExpressionUnary
    (JavaDsl.postfixToRelationalExpression (var "pe"))

javaPostfixExpressionToJavaInclusiveOrExpression :: TBinding (Java.PostfixExpression -> Java.InclusiveOrExpression)
javaPostfixExpressionToJavaInclusiveOrExpression = def "javaPostfixExpressionToJavaInclusiveOrExpression" $
  lambda "pe" $ JavaDsl.postfixToInclusiveOr (var "pe")

javaAdditiveExpressionToJavaExpression :: TBinding (Java.AdditiveExpression -> Java.Expression)
javaAdditiveExpressionToJavaExpression = def "javaAdditiveExpressionToJavaExpression" $
  lambda "ae" $ JavaDsl.additiveToExpression (var "ae")

javaExpressionToJavaUnaryExpression :: TBinding (Java.Expression -> Java.UnaryExpression)
javaExpressionToJavaUnaryExpression = def "javaExpressionToJavaUnaryExpression" $
  lambda "e" $ JavaDsl.expressionToUnary (var "e")

-- =============================================================================
-- Type constructors
-- =============================================================================

javaPrimitiveTypeToJavaType :: TBinding (Java.PrimitiveType -> Java.Type)
javaPrimitiveTypeToJavaType = def "javaPrimitiveTypeToJavaType" $
  lambda "pt" $ JavaDsl.typePrimitive
    (JavaDsl.primitiveTypeWithAnnotations (var "pt") (list ([] :: [TTerm Java.Annotation])))

javaClassTypeToJavaType :: TBinding (Java.ClassType -> Java.Type)
javaClassTypeToJavaType = def "javaClassTypeToJavaType" $
  lambda "ct" $ JavaDsl.typeReference
    (JavaDsl.referenceTypeClassOrInterface
      (JavaDsl.classOrInterfaceTypeClass (var "ct")))

javaTypeVariableToType :: TBinding (Java.TypeVariable -> Java.Type)
javaTypeVariableToType = def "javaTypeVariableToType" $
  lambda "tv" $ JavaDsl.typeReference (JavaDsl.referenceTypeVariable (var "tv"))

javaRefType :: TBinding ([Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.Type)
javaRefType = def "javaRefType" $
  lambda "args" $ lambda "pkg" $ lambda "id" $
    JavaDsl.typeReference
      (JavaDsl.referenceTypeClassOrInterface
        (JavaDsl.classOrInterfaceTypeClass
          (javaClassType @@ var "args" @@ var "pkg" @@ var "id")))

javaClassType :: TBinding ([Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.ClassType)
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

javaTypeVariable :: TBinding (String -> Java.ReferenceType)
javaTypeVariable = def "javaTypeVariable" $
  lambda "v" $ JavaDsl.referenceTypeVariable
    (JavaDsl.typeVariable (list ([] :: [TTerm Java.Annotation]))
      (javaTypeIdentifier @@ (Formatting.capitalize @@ var "v")))

javaBooleanType :: TBinding Java.Type
javaBooleanType = def "javaBooleanType" $
  javaPrimitiveTypeToJavaType @@ JavaDsl.primitiveTypeBoolean

javaIntType :: TBinding Java.Type
javaIntType = def "javaIntType" $
  javaPrimitiveTypeToJavaType @@
    JavaDsl.primitiveTypeNumeric (JavaDsl.numericTypeIntegral JavaDsl.integralTypeInt)

-- =============================================================================
-- Literal expressions
-- =============================================================================

javaBooleanExpression :: TBinding (Bool -> Java.Expression)
javaBooleanExpression = def "javaBooleanExpression" $
  lambda "b" $ javaPrimaryToJavaExpression @@ (javaLiteralToJavaPrimary @@ (javaBoolean @@ var "b"))

javaIntExpression :: TBinding (Int -> Java.Expression)
javaIntExpression = def "javaIntExpression" $
  lambda "i" $ javaPrimaryToJavaExpression @@ (javaLiteralToJavaPrimary @@ (javaInt @@ var "i"))

-- =============================================================================
-- Cast expressions
-- =============================================================================

javaCastExpression :: TBinding (Java.ReferenceType -> Java.UnaryExpression -> Java.CastExpression)
javaCastExpression = def "javaCastExpression" $
  lambda "rt" $ lambda "expr" $
    JavaDsl.castExpressionNotPlusMinus
      (JavaDsl.castExpressionNotPlusMinus_
        (JavaDsl.castExpressionRefAndBounds (var "rt") (list ([] :: [TTerm Java.AdditionalBound])))
        (var "expr"))

javaCastPrimitive :: TBinding (Java.PrimitiveType -> Java.UnaryExpression -> Java.CastExpression)
javaCastPrimitive = def "javaCastPrimitive" $
  lambda "pt" $ lambda "expr" $
    JavaDsl.castExpressionPrimitive
      (JavaDsl.castExpressionPrimitive_
        (JavaDsl.primitiveTypeWithAnnotations (var "pt") (list ([] :: [TTerm Java.Annotation])))
        (var "expr"))

-- =============================================================================
-- Statement constructors
-- =============================================================================

javaReturnStatement :: TBinding (Maybe Java.Expression -> Java.Statement)
javaReturnStatement = def "javaReturnStatement" $
  lambda "mex" $ JavaDsl.statementWithoutTrailing
    (JavaDsl.stmtReturn (JavaDsl.returnStatement (var "mex")))

javaThrowStatement :: TBinding (Java.Expression -> Java.Statement)
javaThrowStatement = def "javaThrowStatement" $
  lambda "e" $ JavaDsl.statementWithoutTrailing
    (JavaDsl.stmtThrow (JavaDsl.throwStatement (var "e")))

javaEmptyStatement :: TBinding Java.Statement
javaEmptyStatement = def "javaEmptyStatement" $
  JavaDsl.statementWithoutTrailing JavaDsl.stmtEmpty

javaAssignmentStatement :: TBinding (Java.LeftHandSide -> Java.Expression -> Java.Statement)
javaAssignmentStatement = def "javaAssignmentStatement" $
  lambda "lhs" $ lambda "rhs" $
    JavaDsl.statementWithoutTrailing
      (JavaDsl.stmtExpression
        (JavaDsl.expressionStatement
          (JavaDsl.stmtExprAssignment
            (JavaDsl.assignment (var "lhs") JavaDsl.assignmentOperatorSimple (var "rhs")))))

javaStatementsToBlock :: TBinding ([Java.Statement] -> Java.Block)
javaStatementsToBlock = def "javaStatementsToBlock" $
  lambda "stmts" $ JavaDsl.block
    (Lists.map (lambda "s" $ JavaDsl.blockStatementStatement (var "s")) (var "stmts"))

-- =============================================================================
-- Lambda constructors
-- =============================================================================

javaLambda :: TBinding (Name -> Java.Expression -> Java.Expression)
javaLambda = def "javaLambda" $
  lambda "v" $ lambda "body" $
    JavaDsl.expressionLambda
      (JavaDsl.lambdaExpression
        (JavaDsl.lambdaParametersSingle (variableToJavaIdentifier @@ var "v"))
        (JavaDsl.lambdaBodyExpression (var "body")))

javaLambdaFromBlock :: TBinding (Name -> Java.Block -> Java.Expression)
javaLambdaFromBlock = def "javaLambdaFromBlock" $
  lambda "v" $ lambda "block" $
    JavaDsl.expressionLambda
      (JavaDsl.lambdaExpression
        (JavaDsl.lambdaParametersSingle (variableToJavaIdentifier @@ var "v"))
        (JavaDsl.lambdaBodyBlock (var "block")))

-- =============================================================================
-- Method/Constructor helpers
-- =============================================================================

javaMethodBody :: TBinding (Maybe [Java.BlockStatement] -> Java.MethodBody)
javaMethodBody = def "javaMethodBody" $
  lambda "mstmts" $ Maybes.cases (var "mstmts")
    JavaDsl.methodBodyNone
    (lambda "stmts" $ JavaDsl.methodBodyBlock (JavaDsl.block (var "stmts")))

javaMethodHeader :: TBinding ([Java.TypeParameter] -> String -> [Java.FormalParameter] -> Java.Result -> Java.MethodHeader)
javaMethodHeader = def "javaMethodHeader" $
  lambda "tparams" $ lambda "methodName" $ lambda "params" $ lambda "result" $
    JavaDsl.methodHeader (var "tparams") (var "result")
      (JavaDsl.methodDeclarator (JavaDsl.identifier (var "methodName")) nothing (var "params"))
      nothing

javaMethodDeclarationToJavaClassBodyDeclaration :: TBinding (Java.MethodDeclaration -> Java.ClassBodyDeclaration)
javaMethodDeclarationToJavaClassBodyDeclaration = def "javaMethodDeclarationToJavaClassBodyDeclaration" $
  lambda "md" $ JavaDsl.classBodyDeclClassMember (JavaDsl.classMemberDeclMethod (var "md"))

javaInterfaceDeclarationToJavaClassBodyDeclaration :: TBinding (Java.NormalInterfaceDeclaration -> Java.ClassBodyDeclaration)
javaInterfaceDeclarationToJavaClassBodyDeclaration = def "javaInterfaceDeclarationToJavaClassBodyDeclaration" $
  lambda "nid" $ JavaDsl.classBodyDeclClassMember
    (JavaDsl.classMemberDeclInterface
      (JavaDsl.interfaceDeclarationNormalInterface (var "nid")))

javaMemberField :: TBinding ([Java.FieldModifier] -> Java.Type -> Java.VariableDeclarator -> Java.ClassBodyDeclaration)
javaMemberField = def "javaMemberField" $
  lambda "mods" $ lambda "jt" $ lambda "v" $
    JavaDsl.classBodyDeclClassMember
      (JavaDsl.classMemberDeclField
        (JavaDsl.fieldDeclaration (var "mods") (JavaDsl.unannType (var "jt")) (list [var "v"])))

javaTypeToJavaFormalParameter :: TBinding (Java.Type -> Name -> Java.FormalParameter)
javaTypeToJavaFormalParameter = def "javaTypeToJavaFormalParameter" $
  lambda "jt" $ lambda "fname" $
    JavaDsl.formalParameterSimple
      (JavaDsl.formalParameterSimple_ (list ([] :: [TTerm Java.VariableModifier]))
        (JavaDsl.unannType (var "jt"))
        (fieldNameToJavaVariableDeclaratorId @@ var "fname"))

javaTypeToJavaResult :: TBinding (Java.Type -> Java.Result)
javaTypeToJavaResult = def "javaTypeToJavaResult" $
  lambda "jt" $ JavaDsl.resultType (JavaDsl.unannType (var "jt"))

javaTypeToJavaTypeArgument :: TBinding (Java.Type -> Java.TypeArgument)
javaTypeToJavaTypeArgument = def "javaTypeToJavaTypeArgument" $
  lambda "t" $ cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt" $ JavaDsl.typeArgumentReference (var "rt"),
    Java._Type_primitive>>: constant $
      JavaDsl.typeArgumentWildcard
        (JavaDsl.wildcard (list ([] :: [TTerm Java.Annotation])) nothing)]

referenceTypeToResult :: TBinding (Java.ReferenceType -> Java.Result)
referenceTypeToResult = def "referenceTypeToResult" $
  lambda "rt" $ javaTypeToJavaResult @@ JavaDsl.typeReference (var "rt")

-- =============================================================================
-- Constructor call
-- =============================================================================

javaConstructorName :: TBinding (Java.Identifier -> Maybe Java.TypeArgumentsOrDiamond -> Java.ClassOrInterfaceTypeToInstantiate)
javaConstructorName = def "javaConstructorName" $
  lambda "id" $ lambda "targs" $
    JavaDsl.classOrInterfaceTypeToInstantiate
      (list [JavaDsl.annotatedIdentifier (list ([] :: [TTerm Java.Annotation])) (var "id")])
      (var "targs")

javaConstructorCall :: TBinding (Java.ClassOrInterfaceTypeToInstantiate -> [Java.Expression] -> Maybe Java.ClassBody -> Java.Expression)
javaConstructorCall = def "javaConstructorCall" $
  lambda "ci" $ lambda "args" $ lambda "mbody" $
    JavaDsl.primaryToExpression
      (JavaDsl.primaryNoNewArray
        (JavaDsl.primaryClassInstance
          (JavaDsl.classInstanceCreationExpression nothing
            (JavaDsl.unqualifiedClassInstanceCreationExpression
              (list ([] :: [TTerm Java.TypeArgument])) (var "ci") (var "args") (var "mbody")))))

-- =============================================================================
-- This
-- =============================================================================

javaThis :: TBinding Java.Expression
javaThis = def "javaThis" $
  JavaDsl.primaryToExpression
    (JavaDsl.primaryNoNewArray JavaDsl.primaryThis)

-- =============================================================================
-- Type helpers
-- =============================================================================

javaTypeParameter :: TBinding (String -> Java.TypeParameter)
javaTypeParameter = def "javaTypeParameter" $
  lambda "v" $ JavaDsl.typeParameter
    (list ([] :: [TTerm Java.TypeParameterModifier]))
    (javaTypeIdentifier @@ var "v")
    nothing

javaTypeIdentifierToJavaTypeArgument :: TBinding (Java.TypeIdentifier -> Java.TypeArgument)
javaTypeIdentifierToJavaTypeArgument = def "javaTypeIdentifierToJavaTypeArgument" $
  lambda "id" $ JavaDsl.typeArgumentReference
    (JavaDsl.referenceTypeVariable
      (JavaDsl.typeVariable (list ([] :: [TTerm Java.Annotation])) (var "id")))

typeParameterToTypeArgument :: TBinding (Java.TypeParameter -> Java.TypeArgument)
typeParameterToTypeArgument = def "typeParameterToTypeArgument" $
  lambda "tp" $ javaTypeIdentifierToJavaTypeArgument @@
    (JavaDsl.typeParameterIdentifier (var "tp"))

typeParameterToReferenceType :: TBinding (Java.TypeParameter -> Java.ReferenceType)
typeParameterToReferenceType = def "typeParameterToReferenceType" $
  lambda "tp" $ javaTypeVariable @@
    (JavaDsl.unIdentifier
      (JavaDsl.unTypeIdentifier
        (JavaDsl.typeParameterIdentifier (var "tp"))))

-- =============================================================================
-- Field helpers
-- =============================================================================

fieldNameToJavaIdentifier :: TBinding (Name -> Java.Identifier)
fieldNameToJavaIdentifier = def "fieldNameToJavaIdentifier" $
  lambda "fname" $ javaIdentifier @@ (Core.unName $ var "fname")

fieldNameToJavaExpression :: TBinding (Name -> Java.Expression)
fieldNameToJavaExpression = def "fieldNameToJavaExpression" $
  lambda "fname" $ JavaDsl.postfixToExpression
    (JavaDsl.postfixExpressionName
      (javaIdentifierToJavaExpressionName @@ (fieldNameToJavaIdentifier @@ var "fname")))

fieldNameToJavaVariableDeclaratorId :: TBinding (Name -> Java.VariableDeclaratorId)
fieldNameToJavaVariableDeclaratorId = def "fieldNameToJavaVariableDeclaratorId" $
  lambda "fname" $ javaVariableDeclaratorId @@ (javaIdentifier @@ (Core.unName $ var "fname"))

fieldNameToJavaVariableDeclarator :: TBinding (Name -> Java.VariableDeclarator)
fieldNameToJavaVariableDeclarator = def "fieldNameToJavaVariableDeclarator" $
  lambda "fname" $ javaVariableDeclarator @@ (javaIdentifier @@ (Core.unName $ var "fname")) @@ nothing

fieldExpression :: TBinding (Java.Identifier -> Java.Identifier -> Java.ExpressionName)
fieldExpression = def "fieldExpression" $
  lambda "varId" $ lambda "fieldId" $
    JavaDsl.expressionName (just (JavaDsl.ambiguousName (list [var "varId"]))) (var "fieldId")

-- =============================================================================
-- Variable helpers
-- =============================================================================

variableToJavaIdentifier :: TBinding (Name -> Java.Identifier)
variableToJavaIdentifier = def "variableToJavaIdentifier" $
  lambda "name" $ lets [
    "v">: Core.unName $ var "name"] $
    Logic.ifElse (Equality.equal (var "v") (string "_"))
      (JavaDsl.identifier (string "ignored"))
      (JavaDsl.identifier (sanitizeJavaName @@ var "v"))

varDeclarationStatement :: TBinding (Java.Identifier -> Java.Expression -> Java.BlockStatement)
varDeclarationStatement = def "varDeclarationStatement" $
  lambda "id" $ lambda "rhs" $
    JavaDsl.blockStatementLocalVariableDeclaration
      (JavaDsl.localVariableDeclarationStatement
        (JavaDsl.localVariableDeclaration
          (list ([] :: [TTerm Java.VariableModifier]))
          JavaDsl.localVariableTypeVar
          (list [javaVariableDeclarator @@ var "id" @@
            (just $ JavaDsl.variableInitializerExpression (var "rhs"))])))

-- =============================================================================
-- Sanitization
-- =============================================================================

sanitizeJavaName :: TBinding (String -> String)
sanitizeJavaName = def "sanitizeJavaName" $
  lambda "name" $
    Logic.ifElse (isEscaped @@ var "name")
      (unescape @@ var "name")
      (Logic.ifElse (Equality.equal (var "name") (string "_"))
        (string "ignored")
        (Formatting.sanitizeWithUnderscores @@ JavaLanguageSource.reservedWords @@ var "name"))

isEscaped :: TBinding (String -> Bool)
isEscaped = def "isEscaped" $
  lambda "s" $ Equality.equal (Strings.charAt (int32 0) (var "s")) (int32 36)

unescape :: TBinding (String -> String)
unescape = def "unescape" $
  lambda "s" $ Strings.fromList (Lists.tail (Strings.toList (var "s")))

-- =============================================================================
-- Package
-- =============================================================================

javaPackageDeclaration :: TBinding (Namespace -> Java.PackageDeclaration)
javaPackageDeclaration = def "javaPackageDeclaration" $
  lambda "ns" $ JavaDsl.packageDeclaration
    (list ([] :: [TTerm Java.PackageModifier]))
    (Lists.map (lambda "s" $ JavaDsl.identifier (var "s")) (Strings.splitOn (string ".") (Module.unNamespace $ var "ns")))

-- =============================================================================
-- Annotations
-- =============================================================================

overrideAnnotation :: TBinding Java.Annotation
overrideAnnotation = def "overrideAnnotation" $
  JavaDsl.annotationMarker
    (JavaDsl.markerAnnotation (javaTypeName @@ JavaDsl.identifier (string "Override")))

-- =============================================================================
-- Method invocations
-- =============================================================================

methodInvocation :: TBinding (Maybe (Either Java.ExpressionName Java.Primary) -> Java.Identifier -> [Java.Expression] -> Java.MethodInvocation)
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

methodInvocationStatic :: TBinding (Java.Identifier -> Java.Identifier -> [Java.Expression] -> Java.MethodInvocation)
methodInvocationStatic = def "methodInvocationStatic" $
  lambda "self" $ lambda "methodName" $ lambda "args" $
    methodInvocation @@
      (just $ left (javaIdentifierToJavaExpressionName @@ var "self")) @@
      var "methodName" @@
      var "args"

methodDeclaration :: TBinding ([Java.MethodModifier] -> [Java.TypeParameter] -> [Java.Annotation]
  -> String -> [Java.FormalParameter] -> Java.Result -> Maybe [Java.BlockStatement] -> Java.ClassBodyDeclaration)
methodDeclaration = def "methodDeclaration" $
  lambda "mods" $ lambda "tparams" $ lambda "anns" $ lambda "methodName" $ lambda "params" $ lambda "result" $ lambda "stmts" $
    javaMethodDeclarationToJavaClassBodyDeclaration @@
      (JavaDsl.methodDeclaration_ (var "anns") (var "mods")
        (javaMethodHeader @@ var "tparams" @@ var "methodName" @@ var "params" @@ var "result")
        (javaMethodBody @@ var "stmts"))

interfaceMethodDeclaration :: TBinding ([Java.InterfaceMethodModifier] -> [Java.TypeParameter] -> String
  -> [Java.FormalParameter] -> Java.Result -> Maybe [Java.BlockStatement] -> Java.InterfaceMemberDeclaration)
interfaceMethodDeclaration = def "interfaceMethodDeclaration" $
  lambda "mods" $ lambda "tparams" $ lambda "methodName" $ lambda "params" $ lambda "result" $ lambda "stmts" $
    JavaDsl.interfaceMemberDeclInterfaceMethod
      (JavaDsl.interfaceMethodDeclaration_ (var "mods")
        (javaMethodHeader @@ var "tparams" @@ var "methodName" @@ var "params" @@ var "result")
        (javaMethodBody @@ var "stmts"))

-- =============================================================================
-- Equals/instanceof
-- =============================================================================

javaEquals :: TBinding (Java.EqualityExpression -> Java.RelationalExpression -> Java.EqualityExpression)
javaEquals = def "javaEquals" $
  lambda "lhs" $ lambda "rhs" $
    JavaDsl.equalityExpressionEqual
      (JavaDsl.equalityExpressionBinary (var "lhs") (var "rhs"))

javaEqualsNull :: TBinding (Java.EqualityExpression -> Java.EqualityExpression)
javaEqualsNull = def "javaEqualsNull" $
  lambda "lhs" $ javaEquals @@ var "lhs" @@
    (javaLiteralToJavaRelationalExpression @@ JavaDsl.literalNull)

javaInstanceOf :: TBinding (Java.RelationalExpression -> Java.ReferenceType -> Java.RelationalExpression)
javaInstanceOf = def "javaInstanceOf" $
  lambda "lhs" $ lambda "rhs" $
    JavaDsl.relationalExpressionInstanceOf
      (JavaDsl.relationalExpressionInstanceOf_ (var "lhs") (var "rhs"))

-- =============================================================================
-- Throw helpers
-- =============================================================================

javaThrowIllegalArgumentException :: TBinding ([Java.Expression] -> Java.Statement)
javaThrowIllegalArgumentException = def "javaThrowIllegalArgumentException" $
  lambda "args" $ javaThrowStatement @@
    (javaConstructorCall @@
      (javaConstructorName @@ JavaDsl.identifier (string "IllegalArgumentException") @@ nothing) @@
      var "args" @@ nothing)

javaThrowIllegalStateException :: TBinding ([Java.Expression] -> Java.Statement)
javaThrowIllegalStateException = def "javaThrowIllegalStateException" $
  lambda "args" $ javaThrowStatement @@
    (javaConstructorCall @@
      (javaConstructorName @@ JavaDsl.identifier (string "IllegalStateException") @@ nothing) @@
      var "args" @@ nothing)

-- =============================================================================
-- Misc
-- =============================================================================

addExpressions :: TBinding ([Java.MultiplicativeExpression] -> Java.AdditiveExpression)
addExpressions = def "addExpressions" $
  lambda "exprs" $ lets [
    "first">: JavaDsl.additiveExpressionUnary (Lists.head (var "exprs")),
    "rest">: Lists.tail (var "exprs")] $
    Lists.foldl
      (lambda "ae" $ lambda "me" $
        JavaDsl.additiveExpressionPlus (JavaDsl.additiveExpressionBinary (var "ae") (var "me")))
      (var "first")
      (var "rest")

-- =============================================================================
-- Name-to-Java mapping
-- =============================================================================

-- | Compute the qualified Java name (TypeIdentifier, ClassTypeQualifier) for a Hydra name.
--   If qualify is True, the namespace is converted to a package qualifier.
--   mlocal is an optional local suffix (for inner class names).
nameToQualifiedJavaName :: TBinding (JavaHelpers.Aliases -> Bool -> Name -> Maybe String
  -> (Java.TypeIdentifier, Java.ClassTypeQualifier))
nameToQualifiedJavaName = def "nameToQualifiedJavaName" $
  lambda "aliases" $ lambda "qualify" $ lambda "name" $ lambda "mlocal" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Module.qualifiedNameNamespace (var "qn"),
    "local">: Module.qualifiedNameLocal (var "qn"),
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
nameToJavaClassType :: TBinding (JavaHelpers.Aliases -> Bool -> [Java.TypeArgument] -> Name -> Maybe String -> Java.ClassType)
nameToJavaClassType = def "nameToJavaClassType" $
  lambda "aliases" $ lambda "qualify" $ lambda "args" $ lambda "name" $ lambda "mlocal" $ lets [
    "result">: nameToQualifiedJavaName @@ var "aliases" @@ var "qualify" @@ var "name" @@ var "mlocal",
    "id">: Pairs.first (var "result"),
    "pkg">: Pairs.second (var "result")] $
    JavaDsl.classType (list ([] :: [TTerm Java.Annotation])) (var "pkg") (var "id") (var "args")

-- | Build a Java ReferenceType from a Hydra name
nameToJavaReferenceType :: TBinding (JavaHelpers.Aliases -> Bool -> [Java.TypeArgument] -> Name -> Maybe String -> Java.ReferenceType)
nameToJavaReferenceType = def "nameToJavaReferenceType" $
  lambda "aliases" $ lambda "qualify" $ lambda "args" $ lambda "name" $ lambda "mlocal" $
    JavaDsl.referenceTypeClassOrInterface
      (JavaDsl.classOrInterfaceTypeClass
        (nameToJavaClassType @@ var "aliases" @@ var "qualify" @@ var "args" @@ var "name" @@ var "mlocal"))

-- | Build a Java Identifier from a Hydra name, using the Aliases for package resolution
nameToJavaName :: TBinding (JavaHelpers.Aliases -> Name -> Java.Identifier)
nameToJavaName = def "nameToJavaName" $
  lambda "aliases" $ lambda "name" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Module.qualifiedNameNamespace (var "qn"),
    "local">: Module.qualifiedNameLocal (var "qn")] $
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
nameToJavaTypeIdentifier :: TBinding (JavaHelpers.Aliases -> Bool -> Name -> Java.TypeIdentifier)
nameToJavaTypeIdentifier = def "nameToJavaTypeIdentifier" $
  lambda "aliases" $ lambda "qualify" $ lambda "name" $
    Pairs.first (nameToQualifiedJavaName @@ var "aliases" @@ var "qualify" @@ var "name" @@ nothing)

-- | Build a Java Type from a type name (for use as formal parameter types)
javaTypeFromTypeName :: TBinding (JavaHelpers.Aliases -> Name -> Java.Type)
javaTypeFromTypeName = def "javaTypeFromTypeName" $
  lambda "aliases" $ lambda "elName" $
    javaTypeVariableToType @@
      (JavaDsl.typeVariable
        (list ([] :: [TTerm Java.Annotation]))
        (nameToJavaTypeIdentifier @@ var "aliases" @@ boolean False @@ var "elName"))

-- =============================================================================
-- Cast helpers
-- =============================================================================

-- | Create a double cast expression: first cast to raw type, then to target type
javaDoubleCastExpression :: TBinding (Java.ReferenceType -> Java.ReferenceType -> Java.UnaryExpression -> Java.CastExpression)
javaDoubleCastExpression = def "javaDoubleCastExpression" $
  lambda "rawRt" $ lambda "targetRt" $ lambda "expr" $ lets [
    "firstCast">: javaCastExpressionToJavaExpression @@ (javaCastExpression @@ var "rawRt" @@ var "expr")] $
    javaCastExpression @@ var "targetRt" @@ (javaExpressionToJavaUnaryExpression @@ var "firstCast")

-- | Create a double cast expression and convert to Java expression
javaDoubleCastExpressionToJavaExpression :: TBinding (Java.ReferenceType -> Java.ReferenceType -> Java.UnaryExpression -> Java.Expression)
javaDoubleCastExpressionToJavaExpression = def "javaDoubleCastExpressionToJavaExpression" $
  lambda "rawRt" $ lambda "targetRt" $ lambda "expr" $
    javaCastExpressionToJavaExpression @@ (javaDoubleCastExpression @@ var "rawRt" @@ var "targetRt" @@ var "expr")

-- =============================================================================
-- Primitive type constants
-- =============================================================================

-- | Java byte primitive type
javaBytePrimitiveType :: TBinding Java.PrimitiveTypeWithAnnotations
javaBytePrimitiveType = def "javaBytePrimitiveType" $
  JavaDsl.primitiveTypeWithAnnotations
    (JavaDsl.primitiveTypeNumeric (JavaDsl.numericTypeIntegral JavaDsl.integralTypeByte))
    (list ([] :: [TTerm Java.Annotation]))

-- =============================================================================
-- Visitor type variable
-- =============================================================================

-- | The reference type for the visitor return type variable "r"
visitorTypeVariable :: TBinding Java.ReferenceType
visitorTypeVariable = def "visitorTypeVariable" $
  javaTypeVariable @@ string "r"

-- =============================================================================
-- Variable name helpers
-- =============================================================================

-- | Look up the Java variable name for a Hydra variable, applying any renames
lookupJavaVarName :: TBinding (JavaHelpers.Aliases -> Name -> Name)
lookupJavaVarName = def "lookupJavaVarName" $
  lambda "aliases" $ lambda "name" $
    Maybes.cases
      (Maps.lookup (var "name")
        (project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases"))
      (var "name")
      (lambda "renamed" $ var "renamed")

-- =============================================================================
-- Variant class name
-- =============================================================================

-- | Compute the class name for a union variant
variantClassName :: TBinding (Bool -> Name -> Name -> Name)
variantClassName = def "variantClassName" $
  lambda "qualify" $ lambda "elName" $ lambda "fname" $ lets [
    "qn">: Names.qualifyName @@ var "elName",
    "ns_">: Module.qualifiedNameNamespace (var "qn"),
    "local">: Module.qualifiedNameLocal (var "qn"),
    "flocal">: Formatting.capitalize @@ (Core.unName $ var "fname"),
    "local1">: Logic.ifElse (var "qualify")
      (Strings.cat2 (Strings.cat2 (var "local") (string ".")) (var "flocal"))
      (Logic.ifElse (Equality.equal (var "flocal") (var "local"))
        (Strings.cat2 (var "flocal") (string "_"))
        (var "flocal"))] $
    Names.unqualifyName @@ Module.qualifiedName (var "ns_") (var "local1")

-- =============================================================================
-- Variable declaration with explicit type
-- =============================================================================

-- | Create a variable declaration statement with an explicit type
variableDeclarationStatement :: TBinding (JavaHelpers.Aliases -> Java.Type -> Java.Identifier -> Java.Expression -> Java.BlockStatement)
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

-- =============================================================================
-- String multiplicative expression
-- =============================================================================

-- | Convert a string to a multiplicative expression (for string concatenation)
javaStringMultiplicativeExpression :: TBinding (String -> Java.MultiplicativeExpression)
javaStringMultiplicativeExpression = def "javaStringMultiplicativeExpression" $
  lambda "s" $ javaLiteralToJavaMultiplicativeExpression @@ (javaString @@ var "s")

-- =============================================================================
-- Annotation helpers
-- =============================================================================

-- | The @SuppressWarnings("unchecked") annotation
suppressWarningsUncheckedAnnotation :: TBinding Java.Annotation
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

-- =============================================================================
-- Method invocation with type args
-- =============================================================================

-- | Create a static method invocation with explicit type arguments
methodInvocationStaticWithTypeArgs :: TBinding (Java.Identifier -> Java.Identifier -> [Java.TypeArgument] -> [Java.Expression] -> Java.MethodInvocation)
methodInvocationStaticWithTypeArgs = def "methodInvocationStaticWithTypeArgs" $
  lambda "self" $ lambda "methodName" $ lambda "targs" $ lambda "args" $ lets [
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantExpression
          (javaIdentifierToJavaExpressionName @@ var "self"))
        (var "targs")
        (var "methodName"))] $
    JavaDsl.methodInvocation_ (var "header") (var "args")

-- =============================================================================
-- Array helpers
-- =============================================================================

-- | Create an array creation expression for primitive byte arrays with an initializer
javaArrayCreation :: TBinding (Java.PrimitiveTypeWithAnnotations -> Maybe Java.ArrayInitializer -> Java.Expression)
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
javaArrayInitializer :: TBinding ([Java.Expression] -> Java.ArrayInitializer)
javaArrayInitializer = def "javaArrayInitializer" $
  lambda "exprs" $ wrap Java._ArrayInitializer
    (list [Lists.map (lambda "e" $ JavaDsl.variableInitializerExpression (var "e")) (var "exprs")])

-- =============================================================================
-- Assignment helpers
-- =============================================================================

-- | Create an assignment statement that assigns a field name to 'this.fieldName'
toAssignStmt :: TBinding (Name -> Java.Statement)
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

-- =============================================================================
-- Type parameter extraction
-- =============================================================================

-- | Extract the string name from a TypeParameter
unTypeParameter :: TBinding (Java.TypeParameter -> String)
unTypeParameter = def "unTypeParameter" $
  lambda "tp" $
    JavaDsl.unIdentifier (JavaDsl.unTypeIdentifier (JavaDsl.typeParameterIdentifier (var "tp")))

-- =============================================================================
-- Aliases construction
-- =============================================================================

-- | Create an initial Aliases for a module with all fields set to empty/default
importAliasesForModule :: TBinding (Module -> JavaHelpers.Aliases)
importAliasesForModule = def "importAliasesForModule" $
  lambda "mod" $
    record JavaHelpers._Aliases [
      JavaHelpers._Aliases_currentNamespace>>: Module.moduleNamespace (var "mod"),
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

-- =============================================================================
-- Class declaration
-- =============================================================================

-- | Build a Java class declaration with optional superclass
javaClassDeclaration :: TBinding (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [Java.ClassModifier]
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

-- =============================================================================
-- Constructor
-- =============================================================================

-- | Build a Java constructor declaration
makeConstructor :: TBinding (JavaHelpers.Aliases -> Name -> Bool -> [Java.FormalParameter]
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

-- =============================================================================
-- Accept method for visitor pattern
-- =============================================================================

-- | Build the accept method for the visitor pattern
toAcceptMethod :: TBinding (Bool -> [Java.TypeParameter] -> Java.ClassBodyDeclaration)
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

-- =============================================================================
-- Array type conversion
-- =============================================================================

-- | Convert a Java Type to an array type
toJavaArrayType :: TBinding (Java.Type -> Flow Graph Java.Type)
toJavaArrayType = def "toJavaArrayType" $
  lambda "t" $ cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt" $
      cases Java._ReferenceType (var "rt") Nothing [
        Java._ReferenceType_classOrInterface>>: lambda "cit" $
          Flows.pure (JavaDsl.typeReference (JavaDsl.referenceTypeArray
            (JavaDsl.arrayType
              (JavaDsl.dims (list [list ([] :: [TTerm Java.Annotation])]))
              (inject Java._ArrayType_Variant Java._ArrayType_Variant_classOrInterface (var "cit"))))),
        Java._ReferenceType_array>>: lambda "at" $ lets [
          "oldDims">: unwrap Java._Dims @@ (project Java._ArrayType Java._ArrayType_dims @@ var "at"),
          "newDims">: JavaDsl.dims (Lists.concat2 (var "oldDims") (list [list ([] :: [TTerm Java.Annotation])])),
          "variant">: project Java._ArrayType Java._ArrayType_variant @@ var "at"] $
          Flows.pure (JavaDsl.typeReference (JavaDsl.referenceTypeArray
            (JavaDsl.arrayType (var "newDims") (var "variant")))),
        Java._ReferenceType_variable>>: constant $
          Flows.fail (string "don't know how to make Java reference type into array type")],
    Java._Type_primitive>>: constant $
      Flows.fail (string "don't know how to make Java type into array type")]

-- =============================================================================
-- Type to reference type
-- =============================================================================

-- | Extract the reference type from a Java type, failing if it's a primitive type
javaTypeToJavaReferenceType :: TBinding (Java.Type -> Flow Graph Java.ReferenceType)
javaTypeToJavaReferenceType = def "javaTypeToJavaReferenceType" $
  lambda "t" $ cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt" $ Flows.pure (var "rt"),
    Java._Type_primitive>>: constant $
      Flows.fail (string "expected a Java reference type")]

-- =============================================================================
-- Reference type to raw type
-- =============================================================================

-- | Extract the raw type (without type arguments) from a reference type
javaReferenceTypeToRawType :: TBinding (Java.ReferenceType -> Java.ReferenceType)
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

-- =============================================================================
-- Type parameter add
-- =============================================================================

-- | Add a reference type as a type argument to an existing Java type
addJavaTypeParameter :: TBinding (Java.ReferenceType -> Java.Type -> Flow Graph Java.Type)
addJavaTypeParameter = def "addJavaTypeParameter" $
  lambda "rt" $ lambda "t" $ cases Java._Type (var "t") Nothing [
    Java._Type_reference>>: lambda "rt1" $
      cases Java._ReferenceType (var "rt1") Nothing [
        Java._ReferenceType_classOrInterface>>: lambda "cit" $
          cases Java._ClassOrInterfaceType (var "cit") Nothing [
            Java._ClassOrInterfaceType_class>>: lambda "ct" $ lets [
              "anns">: project Java._ClassType Java._ClassType_annotations @@ var "ct",
              "qual">: project Java._ClassType Java._ClassType_qualifier @@ var "ct",
              "id">: project Java._ClassType Java._ClassType_identifier @@ var "ct",
              "args">: project Java._ClassType Java._ClassType_arguments @@ var "ct"] $
              Flows.pure (JavaDsl.typeReference
                (JavaDsl.referenceTypeClassOrInterface
                  (JavaDsl.classOrInterfaceTypeClass
                    (JavaDsl.classType (var "anns") (var "qual") (var "id")
                      (Lists.concat2 (var "args") (list [JavaDsl.typeArgumentReference (var "rt")])))))),
            Java._ClassOrInterfaceType_interface>>: constant $
              Flows.fail (string "expected a Java class type")],
        Java._ReferenceType_variable>>: lambda "tv" $
          Flows.pure (javaTypeVariableToType @@ var "tv"),
        Java._ReferenceType_array>>: constant $
          Flows.fail (string "expected a Java class or interface type, or a variable")],
    Java._Type_primitive>>: constant $
      Flows.fail (string "expected a reference type")]

-- =============================================================================
-- Aliases modifiers
-- =============================================================================

-- | Generate a unique variable name that doesn't conflict with in-scope names
uniqueVarName :: TBinding (JavaHelpers.Aliases -> Name -> Name)
uniqueVarName = def "uniqueVarName" $
  lambda "aliases" $ lambda "name" $
    Logic.ifElse
      (Sets.member (var "name")
        (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases"))
      (uniqueVarName_go @@ var "aliases" @@ Core.unName (var "name") @@ int32 2)
      (var "name")

-- | Helper for uniqueVarName
uniqueVarName_go :: TBinding (JavaHelpers.Aliases -> String -> Int -> Name)
uniqueVarName_go = def "uniqueVarName_go" $
  lambda "aliases" $ lambda "base" $ lambda "n" $ lets [
    "candidate">: wrap _Name (Strings.cat2 (var "base") (Literals.showInt32 (var "n")))] $
    Logic.ifElse
      (Sets.member (var "candidate")
        (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases"))
      (uniqueVarName_go @@ var "aliases" @@ var "base" @@ Math.add (var "n") (int32 1))
      (var "candidate")

-- | Add a variable to the in-scope set and return updated aliases
addInScopeVar :: TBinding (Name -> JavaHelpers.Aliases -> JavaHelpers.Aliases)
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
addInScopeVars :: TBinding ([Name] -> JavaHelpers.Aliases -> JavaHelpers.Aliases)
addInScopeVars = def "addInScopeVars" $
  lambda "names" $ lambda "aliases" $
    Lists.foldl (lambda "a" $ lambda "n" $ addInScopeVar @@ var "n" @@ var "a")
      (var "aliases") (var "names")

-- | Register a variable rename
addVarRename :: TBinding (Name -> Name -> JavaHelpers.Aliases -> JavaHelpers.Aliases)
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
