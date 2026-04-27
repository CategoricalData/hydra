-- | C++ utilities for constructing C++ syntax trees.
-- Provides functions for building common C++ AST patterns, and the CppEnvironment type.

module Hydra.Sources.Cpp.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as KAnnotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Cpp.Syntax as Cpp
import qualified Hydra.Sources.Cpp.Syntax as CppSyntax


-- Phantom type for CppEnvironment, defined in Hydra.Sources.Cpp.Environment
data CppEnvironment

_CppEnvironment :: Name
_CppEnvironment = Name "hydra.cpp.environment.CppEnvironment"

_CppEnvironment_namespaces :: Name
_CppEnvironment_namespaces = Name "namespaces"

_CppEnvironment_boundTypeVariables :: Name
_CppEnvironment_boundTypeVariables = Name "boundTypeVariables"


-- Term-level definitions

def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.cpp.utils"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [],
            moduleTypeDependencies = (CppSyntax.ns:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "C++ utilities for constructing C++ syntax trees"}
  where
    definitions = [
      toDefinition constParameter,
      toDefinition cppClassDeclaration,
      toDefinition cppEnumDeclaration,
      toDefinition cppEnumForwardDeclaration,
      toDefinition cppPostfixExpressionToCppExpression,
      toDefinition cppPrimaryExpressionToCppExpression,
      toDefinition cppUnaryExpressionToCppExpression,
      toDefinition cppUnaryExpressionToCppLogicalOrExpression,
      toDefinition createCastExpr,
      toDefinition createCompoundStmt,
      toDefinition createConstRefType,
      toDefinition createConstType,
      toDefinition createConstructorBody,
      toDefinition createEnumAccessExpr,
      toDefinition createFunctionCallExpr,
      toDefinition createHeaderFile,
      toDefinition createIdentifierExpr,
      toDefinition createLambdaExpr,
      toDefinition createLiteralBoolExpr,
      toDefinition createLiteralIntExpr,
      toDefinition createLiteralStringExpr,
      toDefinition createMemberAccessExpr,
      toDefinition createQualifiedType,
      toDefinition createReferenceType,
      toDefinition createReturnStmt,
      toDefinition createReturnVoidStmt,
      toDefinition createTemplateType,
      toDefinition createThisExpr,
      toDefinition createThrowStmt,
      toDefinition createTypeIdNameCall,
      toDefinition createTypeNameExpr,
      toDefinition createVariantExpr,
      toDefinition emptyFunctionBody,
      toDefinition extractPostfixExpression,
      toDefinition memberSpecificationProtected,
      toDefinition memberSpecificationPublic,
      toDefinition stringExpression,
      toDefinition toConstType,
      toDefinition unnamedParameter]

-- | Create a const reference parameter
constParameter :: TTermDefinition (String -> Cpp.TypeExpression -> Cpp.Parameter)
constParameter = def "constParameter" $
  doc "Create a const reference parameter" $
  lambdas ["name", "typ"] $
    record Cpp._Parameter [
      Cpp._Parameter_type>>:
        inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
          record Cpp._QualifiedType [
            Cpp._QualifiedType_baseType>>:
              inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                record Cpp._QualifiedType [
                  Cpp._QualifiedType_baseType>>: var "typ",
                  Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit],
            Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
      Cpp._Parameter_name>>: var "name",
      Cpp._Parameter_unnamed>>: false,
      Cpp._Parameter_defaultValue>>: nothing]

-- | Create a C++ class declaration
cppClassDeclaration :: TTermDefinition (String -> [Cpp.BaseSpecifier] -> Maybe Cpp.ClassBody -> Cpp.Declaration)
cppClassDeclaration = def "cppClassDeclaration" $
  doc "Create a C++ class declaration" $
  lambdas ["name", "baseSpecs", "mbody"] $
    inject Cpp._Declaration Cpp._Declaration_class $
      record Cpp._ClassDeclaration [
        Cpp._ClassDeclaration_specifier>>:
          record Cpp._ClassSpecifier [
            Cpp._ClassSpecifier_key>>: inject Cpp._ClassKey Cpp._ClassKey_class unit,
            Cpp._ClassSpecifier_name>>: var "name",
            Cpp._ClassSpecifier_inheritance>>: var "baseSpecs"],
        Cpp._ClassDeclaration_body>>: var "mbody"]

-- | Create a C++ enum class declaration
cppEnumDeclaration :: TTermDefinition (String -> Maybe Cpp.ClassBody -> Cpp.Declaration)
cppEnumDeclaration = def "cppEnumDeclaration" $
  doc "Create a C++ enum class declaration" $
  lambdas ["name", "mbody"] $
    inject Cpp._Declaration Cpp._Declaration_class $
      record Cpp._ClassDeclaration [
        Cpp._ClassDeclaration_specifier>>:
          record Cpp._ClassSpecifier [
            Cpp._ClassSpecifier_key>>: inject Cpp._ClassKey Cpp._ClassKey_enumClass unit,
            Cpp._ClassSpecifier_name>>: var "name",
            Cpp._ClassSpecifier_inheritance>>: list ([] :: [TTerm Cpp.BaseSpecifier])],
        Cpp._ClassDeclaration_body>>: var "mbody"]

-- | Create a C++ enum class forward declaration
cppEnumForwardDeclaration :: TTermDefinition (String -> Cpp.Declaration)
cppEnumForwardDeclaration = def "cppEnumForwardDeclaration" $
  doc "Create a C++ enum class forward declaration" $
  lambda "name" $
    inject Cpp._Declaration Cpp._Declaration_class $
      record Cpp._ClassDeclaration [
        Cpp._ClassDeclaration_specifier>>:
          record Cpp._ClassSpecifier [
            Cpp._ClassSpecifier_key>>: inject Cpp._ClassKey Cpp._ClassKey_enumClass unit,
            Cpp._ClassSpecifier_name>>: var "name",
            Cpp._ClassSpecifier_inheritance>>: list ([] :: [TTerm Cpp.BaseSpecifier])],
        Cpp._ClassDeclaration_body>>: nothing]

-- | Convert a PostfixExpression to an Expression
cppPostfixExpressionToCppExpression :: TTermDefinition (Cpp.PostfixExpression -> Cpp.Expression)
cppPostfixExpressionToCppExpression = def "cppPostfixExpressionToCppExpression" $
  doc "Convert a PostfixExpression to an Expression" $
  lambda "pf" $
    cppUnaryExpressionToCppExpression @@
      (inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix $ var "pf")

-- | Convert a PrimaryExpression to an Expression
cppPrimaryExpressionToCppExpression :: TTermDefinition (Cpp.PrimaryExpression -> Cpp.Expression)
cppPrimaryExpressionToCppExpression = def "cppPrimaryExpressionToCppExpression" $
  doc "Convert a PrimaryExpression to an Expression" $
  lambda "prim" $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $ var "prim")

-- | Convert a UnaryExpression to an Expression
cppUnaryExpressionToCppExpression :: TTermDefinition (Cpp.UnaryExpression -> Cpp.Expression)
cppUnaryExpressionToCppExpression = def "cppUnaryExpressionToCppExpression" $
  doc "Convert a UnaryExpression to an Expression" $
  lambda "ue" $
    inject Cpp._Expression Cpp._Expression_assignment $
      inject Cpp._AssignmentExpression Cpp._AssignmentExpression_conditional $
        inject Cpp._ConditionalExpression Cpp._ConditionalExpression_logicalOr $
          cppUnaryExpressionToCppLogicalOrExpression @@ var "ue"

-- | Convert a UnaryExpression to a LogicalOrExpression
cppUnaryExpressionToCppLogicalOrExpression :: TTermDefinition (Cpp.UnaryExpression -> Cpp.LogicalOrExpression)
cppUnaryExpressionToCppLogicalOrExpression = def "cppUnaryExpressionToCppLogicalOrExpression" $
  doc "Convert a UnaryExpression to a LogicalOrExpression" $
  lambda "ue" $
    inject Cpp._LogicalOrExpression Cpp._LogicalOrExpression_logicalAnd $
      inject Cpp._LogicalAndExpression Cpp._LogicalAndExpression_inclusiveOr $
        inject Cpp._InclusiveOrExpression Cpp._InclusiveOrExpression_exclusiveOr $
          inject Cpp._ExclusiveOrExpression Cpp._ExclusiveOrExpression_and $
            inject Cpp._AndExpression Cpp._AndExpression_equality $
              inject Cpp._EqualityExpression Cpp._EqualityExpression_relational $
                inject Cpp._RelationalExpression Cpp._RelationalExpression_shift $
                  inject Cpp._ShiftExpression Cpp._ShiftExpression_additive $
                    inject Cpp._AdditiveExpression Cpp._AdditiveExpression_multiplicative $
                      inject Cpp._MultiplicativeExpression Cpp._MultiplicativeExpression_unary $
                        var "ue"

-- | Create a cast expression
createCastExpr :: TTermDefinition (Cpp.TypeExpression -> Cpp.Expression -> Cpp.Expression)
createCastExpr = def "createCastExpr" $
  doc "Create a cast expression" $
  lambdas ["targetType", "expr"] $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_parenthesized $
        cppPrimaryExpressionToCppExpression @@
          (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_parenthesized $ var "expr"))

-- | Create a compound statement
createCompoundStmt :: TTermDefinition ([Cpp.Statement] -> Cpp.CompoundStatement)
createCompoundStmt = def "createCompoundStmt" $
  doc "Create a compound statement" $
  lambda "stmts" $ wrap Cpp._CompoundStatement $ var "stmts"

-- | Create a const reference type
createConstRefType :: TTermDefinition (Cpp.TypeExpression -> Cpp.TypeExpression)
createConstRefType = def "createConstRefType" $
  doc "Create a const reference type" $
  lambda "baseType" $
    createReferenceType @@ (createConstType @@ var "baseType")

-- | Create a const-qualified type
createConstType :: TTermDefinition (Cpp.TypeExpression -> Cpp.TypeExpression)
createConstType = def "createConstType" $
  doc "Create a const-qualified type" $
  lambda "baseType" $
    createQualifiedType @@ var "baseType" @@ (inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit)

-- | Create a constructor body (default if no params, empty otherwise)
createConstructorBody :: TTermDefinition ([Cpp.Parameter] -> Cpp.FunctionBody)
createConstructorBody = def "createConstructorBody" $
  doc "Create a constructor body (default if no params, empty otherwise)" $
  lambda "params" $
    Logic.ifElse (Lists.null $ var "params")
      (inject Cpp._FunctionBody Cpp._FunctionBody_default unit)
      emptyFunctionBody

-- | Create an enum access expression (e.g., EnumName.valueName)
createEnumAccessExpr :: TTermDefinition (String -> String -> Cpp.Expression)
createEnumAccessExpr = def "createEnumAccessExpr" $
  doc "Create an enum access expression" $
  lambdas ["enumName", "valueName"] $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_memberAccess $
        record Cpp._MemberAccessOperation [
          Cpp._MemberAccessOperation_object>>:
            inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
              inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ var "enumName",
          Cpp._MemberAccessOperation_member>>: var "valueName"])

-- | Create a function call expression
createFunctionCallExpr :: TTermDefinition (String -> [Cpp.Expression] -> Cpp.Expression)
createFunctionCallExpr = def "createFunctionCallExpr" $
  doc "Create a function call expression" $
  lambdas ["funcName", "args"] $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
        record Cpp._FunctionCallOperation [
          Cpp._FunctionCallOperation_function>>:
            inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
              inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ var "funcName",
          Cpp._FunctionCallOperation_arguments>>: var "args"])

-- | Create a header file with pragma once
createHeaderFile :: TTermDefinition ([Cpp.IncludeDirective] -> [Cpp.Declaration] -> Cpp.Program)
createHeaderFile = def "createHeaderFile" $
  doc "Create a header file with pragma once" $
  lambdas ["includes", "decls"] $
    record Cpp._Program [
      Cpp._Program_preprocessorDirectives>>:
        list [inject Cpp._PreprocessorDirective Cpp._PreprocessorDirective_pragma $
          record Cpp._PragmaDirective [Cpp._PragmaDirective_content>>: string "once"]],
      Cpp._Program_includes>>: var "includes",
      Cpp._Program_declarations>>: var "decls"]

-- | Create an identifier expression
createIdentifierExpr :: TTermDefinition (String -> Cpp.Expression)
createIdentifierExpr = def "createIdentifierExpr" $
  doc "Create an identifier expression" $
  lambda "name" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ var "name")

-- | Create a lambda expression
createLambdaExpr :: TTermDefinition ([Cpp.Parameter] -> Cpp.Expression -> Cpp.Expression)
createLambdaExpr = def "createLambdaExpr" $
  doc "Create a lambda expression" $
  lambdas ["params", "body"] $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_lambda $
          record Cpp._LambdaExpression [
            Cpp._LambdaExpression_captures>>:
              inject Cpp._CaptureList Cpp._CaptureList_captures $
                list ([] :: [TTerm Cpp.Capture]),
            Cpp._LambdaExpression_parameters>>: var "params",
            Cpp._LambdaExpression_returnType>>: nothing,
            Cpp._LambdaExpression_body>>:
              wrap Cpp._CompoundStatement $
                list [inject Cpp._Statement Cpp._Statement_expression $ var "body"]])

-- | Create a boolean literal expression
createLiteralBoolExpr :: TTermDefinition (Bool -> Cpp.Expression)
createLiteralBoolExpr = def "createLiteralBoolExpr" $
  doc "Create a boolean literal expression" $
  lambda "val" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_boolean $
          wrap Cpp._BooleanLiteral $ var "val")

-- | Create an integer literal expression
createLiteralIntExpr :: TTermDefinition (I.Int32 -> Cpp.Expression)
createLiteralIntExpr = def "createLiteralIntExpr" $
  doc "Create an integer literal expression" $
  lambda "val" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_integer $
          inject Cpp._IntegerLiteral Cpp._IntegerLiteral_decimal $
            var "val")

-- | Create a string literal expression
createLiteralStringExpr :: TTermDefinition (String -> Cpp.Expression)
createLiteralStringExpr = def "createLiteralStringExpr" $
  doc "Create a string literal expression" $
  lambda "val" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_string $
          wrap Cpp._StringLiteral $ var "val")

-- | Create a member access expression (e.g., obj.member)
createMemberAccessExpr :: TTermDefinition (Cpp.Expression -> String -> Cpp.Expression)
createMemberAccessExpr = def "createMemberAccessExpr" $
  doc "Create a member access expression" $
  lambdas ["objExpr", "member"] $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_memberAccess $
        record Cpp._MemberAccessOperation [
          Cpp._MemberAccessOperation_object>>: extractPostfixExpression @@ var "objExpr",
          Cpp._MemberAccessOperation_member>>: var "member"])

-- | Create a qualified type with a qualifier
createQualifiedType :: TTermDefinition (Cpp.TypeExpression -> Cpp.TypeQualifier -> Cpp.TypeExpression)
createQualifiedType = def "createQualifiedType" $
  doc "Create a qualified type with a qualifier" $
  lambdas ["baseType", "qualifier"] $
    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
      record Cpp._QualifiedType [
        Cpp._QualifiedType_baseType>>: var "baseType",
        Cpp._QualifiedType_qualifier>>: var "qualifier"]

-- | Create a reference type
createReferenceType :: TTermDefinition (Cpp.TypeExpression -> Cpp.TypeExpression)
createReferenceType = def "createReferenceType" $
  doc "Create a reference type" $
  lambda "baseType" $
    createQualifiedType @@ var "baseType" @@ (inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit)

-- | Create a return statement with a value
createReturnStmt :: TTermDefinition (Cpp.Expression -> Cpp.Statement)
createReturnStmt = def "createReturnStmt" $
  doc "Create a return statement with a value" $
  lambda "expr" $
    inject Cpp._Statement Cpp._Statement_jump $
      inject Cpp._JumpStatement Cpp._JumpStatement_returnValue $ var "expr"

-- | Create a void return statement
createReturnVoidStmt :: TTermDefinition Cpp.Statement
createReturnVoidStmt = def "createReturnVoidStmt" $
  doc "Create a void return statement" $
  inject Cpp._Statement Cpp._Statement_jump $
    inject Cpp._JumpStatement Cpp._JumpStatement_returnVoid unit

-- | Create a template type (e.g., std::shared_ptr<T>)
createTemplateType :: TTermDefinition (String -> [Cpp.TypeExpression] -> Cpp.TypeExpression)
createTemplateType = def "createTemplateType" $
  doc "Create a template type" $
  lambdas ["name", "args"] $
    inject Cpp._TypeExpression Cpp._TypeExpression_template $
      record Cpp._TemplateType [
        Cpp._TemplateType_name>>: var "name",
        Cpp._TemplateType_arguments>>:
          Lists.map
            (lambda "a" $ inject Cpp._TemplateArgument Cpp._TemplateArgument_type $ var "a")
            (var "args")]

-- | Create a *this expression
createThisExpr :: TTermDefinition Cpp.Expression
createThisExpr = def "createThisExpr" $
  doc "Create a *this expression" $
  cppPostfixExpressionToCppExpression @@
    (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
      inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ string "*this")

-- | Create a throw statement
createThrowStmt :: TTermDefinition (String -> Cpp.Expression -> Cpp.Statement)
createThrowStmt = def "createThrowStmt" $
  doc "Create a throw statement" $
  lambdas ["exceptionType", "arg"] $
    inject Cpp._Statement Cpp._Statement_jump $
      inject Cpp._JumpStatement Cpp._JumpStatement_throw $
        createFunctionCallExpr @@ var "exceptionType" @@ list [var "arg"]

-- | Create a typeid(...).name() call expression
createTypeIdNameCall :: TTermDefinition Cpp.Expression
createTypeIdNameCall = def "createTypeIdNameCall" $
  doc "Create a typeid(...).name() call expression" $
  cppPostfixExpressionToCppExpression @@
    (inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
      record Cpp._FunctionCallOperation [
        Cpp._FunctionCallOperation_function>>:
          inject Cpp._PostfixExpression Cpp._PostfixExpression_memberAccess $
            record Cpp._MemberAccessOperation [
              Cpp._MemberAccessOperation_object>>:
                inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
                  record Cpp._FunctionCallOperation [
                    Cpp._FunctionCallOperation_function>>:
                      inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ string "typeid",
                    Cpp._FunctionCallOperation_arguments>>:
                      list [cppPrimaryExpressionToCppExpression @@
                        (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_parenthesized $ asTerm createThisExpr)]],
              Cpp._MemberAccessOperation_member>>: string "name"],
        Cpp._FunctionCallOperation_arguments>>: list ([] :: [TTerm Cpp.Expression])])

-- | Create a type name expression
createTypeNameExpr :: TTermDefinition (String -> Cpp.Expression)
createTypeNameExpr = def "createTypeNameExpr" $
  doc "Create a type name expression" $
  lambda "typeName" $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ var "typeName")

-- | Create a variant expression
createVariantExpr :: TTermDefinition Cpp.Expression
createVariantExpr = def "createVariantExpr" $
  doc "Create a variant expression" $
  cppPostfixExpressionToCppExpression @@
    (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
      inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ string "variant")

-- | An empty function body
emptyFunctionBody :: TTermDefinition Cpp.FunctionBody
emptyFunctionBody = def "emptyFunctionBody" $
  doc "An empty function body" $
  inject Cpp._FunctionBody Cpp._FunctionBody_compound $
    wrap Cpp._CompoundStatement $ list ([] :: [TTerm Cpp.Statement])

-- | Extract the PostfixExpression from a fully-wrapped Expression
extractPostfixExpression :: TTermDefinition (Cpp.Expression -> Cpp.PostfixExpression)
extractPostfixExpression = def "extractPostfixExpression" $
  doc "Extract the PostfixExpression from a fully-wrapped Expression" $
  lambda "expr" $
    match Cpp._Expression (Just defaultPostfix) [
      Cpp._Expression_assignment>>: lambda "a" $
        match Cpp._AssignmentExpression (Just defaultPostfix) [
          Cpp._AssignmentExpression_conditional>>: lambda "c" $
            match Cpp._ConditionalExpression (Just defaultPostfix) [
              Cpp._ConditionalExpression_logicalOr>>: lambda "lo" $
                match Cpp._LogicalOrExpression (Just defaultPostfix) [
                  Cpp._LogicalOrExpression_logicalAnd>>: lambda "la" $
                    match Cpp._LogicalAndExpression (Just defaultPostfix) [
                      Cpp._LogicalAndExpression_inclusiveOr>>: lambda "io" $
                        match Cpp._InclusiveOrExpression (Just defaultPostfix) [
                          Cpp._InclusiveOrExpression_exclusiveOr>>: lambda "xo" $
                            match Cpp._ExclusiveOrExpression (Just defaultPostfix) [
                              Cpp._ExclusiveOrExpression_and>>: lambda "ae" $
                                match Cpp._AndExpression (Just defaultPostfix) [
                                  Cpp._AndExpression_equality>>: lambda "eq" $
                                    match Cpp._EqualityExpression (Just defaultPostfix) [
                                      Cpp._EqualityExpression_relational>>: lambda "re" $
                                        match Cpp._RelationalExpression (Just defaultPostfix) [
                                          Cpp._RelationalExpression_shift>>: lambda "sh" $
                                            match Cpp._ShiftExpression (Just defaultPostfix) [
                                              Cpp._ShiftExpression_additive>>: lambda "ad" $
                                                match Cpp._AdditiveExpression (Just defaultPostfix) [
                                                  Cpp._AdditiveExpression_multiplicative>>: lambda "mu" $
                                                    match Cpp._MultiplicativeExpression (Just defaultPostfix) [
                                                      Cpp._MultiplicativeExpression_unary>>: lambda "ue" $
                                                        match Cpp._UnaryExpression (Just defaultPostfix) [
                                                          Cpp._UnaryExpression_postfix>>: lambda "pf" $ var "pf"]
                                                        @@ var "ue"]
                                                    @@ var "mu"]
                                                @@ var "ad"]
                                            @@ var "sh"]
                                        @@ var "re"]
                                    @@ var "eq"]
                                @@ var "ae"]
                            @@ var "xo"]
                        @@ var "io"]
                    @@ var "la"]
                @@ var "lo"]
            @@ var "c"]
        @@ var "a"]
    @@ var "expr"
  where
    defaultPostfix =
      inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier $ string "error"

-- | Protected access specifier member specification
memberSpecificationProtected :: TTermDefinition Cpp.MemberSpecification
memberSpecificationProtected = def "memberSpecificationProtected" $
  doc "Protected access specifier member specification" $
  inject Cpp._MemberSpecification Cpp._MemberSpecification_accessLabel $
    inject Cpp._AccessSpecifier Cpp._AccessSpecifier_protected unit

-- | Public access specifier member specification
memberSpecificationPublic :: TTermDefinition Cpp.MemberSpecification
memberSpecificationPublic = def "memberSpecificationPublic" $
  doc "Public access specifier member specification" $
  inject Cpp._MemberSpecification Cpp._MemberSpecification_accessLabel $
    inject Cpp._AccessSpecifier Cpp._AccessSpecifier_public unit

-- | Create a string expression
stringExpression :: TTermDefinition (String -> Cpp.Expression)
stringExpression = def "stringExpression" $
  doc "Create a string expression" $
  lambda "s" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_string $
          wrap Cpp._StringLiteral $ var "s")

-- | Add const qualifier to a type
toConstType :: TTermDefinition (Cpp.TypeExpression -> Cpp.TypeExpression)
toConstType = def "toConstType" $
  doc "Add const qualifier to a type" $
  lambda "baseType" $
    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
      record Cpp._QualifiedType [
        Cpp._QualifiedType_baseType>>: var "baseType",
        Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit]

-- | Create an unnamed parameter
unnamedParameter :: TTermDefinition (String -> Cpp.TypeExpression -> Cpp.Parameter)
unnamedParameter = def "unnamedParameter" $
  doc "Create an unnamed parameter" $
  lambdas ["name", "typ"] $
    record Cpp._Parameter [
      Cpp._Parameter_type>>: var "typ",
      Cpp._Parameter_name>>: var "name",
      Cpp._Parameter_unnamed>>: true,
      Cpp._Parameter_defaultValue>>: nothing]
