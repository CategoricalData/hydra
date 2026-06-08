
-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Sources.Cpp.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Dependencies   as Dependencies
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Predicates    as Predicates
import qualified Hydra.Sources.Kernel.Terms.Resolution    as Resolution
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import           Prelude hiding ((++))
import qualified Data.List                  as L

import qualified Data.Map as M
import qualified Data.Set as S

-- Additional imports for Cpp AST
import qualified Hydra.Cpp.Syntax as Cpp
import qualified Hydra.Sources.Cpp.Serde as CppSerde
import qualified Hydra.Sources.Cpp.Syntax as CppSyntax
import qualified Hydra.Sources.Cpp.Language as CppLanguageSource


ns :: ModuleName
ns = ModuleName "hydra.cpp.coder"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([moduleName CppLanguageSource.module_,
      CppSerde.ns,
      Formatting.ns, Names.ns, Dependencies.ns, Strip.ns, Environment.ns, Predicates.ns, Resolution.ns, Lexical.ns,
      ShowCore.ns, Annotations.ns, Sorting.ns, SerializationSource.ns,
      moduleName DecodeCore.module_, moduleName EncodeCore.module_] L.++ (CppSyntax.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = descriptionMetadata (Just "C++ code generator: converts Hydra modules to C++ header files")}
  where
    definitions = [
      -- Cpp AST helper functions
      toDefinition cppClassDeclaration,
      toDefinition cppEnumDeclaration,
      toDefinition cppEnumForwardDeclaration,
      toDefinition memberSpecificationPublic,
      toDefinition memberSpecificationProtected,
      toDefinition createTemplateType,
      toDefinition createIdentifierExpr,
      toDefinition createLiteralBoolExpr,
      toDefinition createLiteralIntExpr,
      toDefinition createFunctionCallExpr,
      toDefinition createHeaderFile,
      toDefinition createConstructorBody,
      toDefinition constParameter,
      toDefinition unnamedParameter,
      toDefinition toConstType,
      toDefinition createTypeIdNameCall,
      toDefinition createThrowStmt,
      -- Expression chain helpers
      toDefinition cppPrimaryExpressionToCppExpression,
      toDefinition cppUnaryExpressionToCppExpression,
      toDefinition cppPostfixExpressionToCppExpression,
      toDefinition cppUnaryExpressionToCppLogicalOrExpression,
      -- Naming functions
      toDefinition encodeName,
      toDefinition encodeNamespace,
      toDefinition encodeFieldName,
      toDefinition encodeEnumValue,
      toDefinition sanitizeCppName,
      toDefinition className,
      toDefinition variantName,
      toDefinition visitorName,
      toDefinition partialVisitorName,
      toDefinition fwdHeaderName,
      toDefinition namespaceDecl,
      toDefinition createTypeReference,
      -- Type encoding
      toDefinition encodeLiteralType,
      toDefinition encodeType,
      toDefinition encodeForallType,
      toDefinition encodeFunctionType,
      toDefinition encodeApplicationType,
      toDefinition encodeTypeAlias,
      toDefinition encodeTypeDefinition,
      -- Struct/union/enum encoding
      toDefinition encodeFieldType,
      toDefinition encodeRecordType,
      toDefinition encodeUnionType,
      toDefinition encodeEnumType,
      toDefinition encodeVariantType,
      toDefinition encodeWrappedType,
      -- Visitor pattern
      toDefinition createVisitorInterface,
      toDefinition createPartialVisitorInterface,
      toDefinition createUnionBaseClass,
      toDefinition createVariantClass,
      toDefinition createAcceptImplementation,
      toDefinition generateForwardDeclarations,
      toDefinition createLessThanOperator,
      -- File generation
      toDefinition serializeHeaderFile,
      toDefinition bindingNameToFilePath,
      toDefinition findIncludes,
      toDefinition findTypeDependencies,
      toDefinition gatherMetadata,
      toDefinition isStdContainerType,
      toDefinition isStructType,
      toDefinition isTemplateType,
      -- Module entry point
      toDefinition generateTypeFile,
      toDefinition generateTypeFiles,
      toDefinition moduleToCpp]


-- ============================================================================
-- Cpp AST helper functions
-- ============================================================================

-- | Convert a binding name to a file path
bindingNameToFilePath :: TypedTermDefinition (Name -> FilePath)
bindingNameToFilePath = def "bindingNameToFilePath" $
  lambda "name" $
    Names.nameToFilePath
      @@ (inject _CaseConvention _CaseConvention_lowerSnake unit)
      @@ (inject _CaseConvention _CaseConvention_lowerSnake unit)
      @@ wrap _FileExtension (string "h")
      @@ var "name"

-- | Get the class name from a fully qualified Name
className :: TypedTermDefinition (Name -> String)
className = def "className" $
  lambda "name" $
    sanitizeCppName @@ (Names.localNameOf @@ var "name")

-- | Create a const reference parameter
constParameter :: TypedTermDefinition (String -> Cpp.TypeExpression -> Cpp.Parameter)
constParameter = def "constParameter" $
  lambda "name" $ lambda "typ" $
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
      Cpp._Parameter_unnamed>>: boolean False,
      Cpp._Parameter_defaultValue>>: nothing]

-- | Create a class declaration
cppClassDeclaration :: TypedTermDefinition (String -> [Cpp.BaseSpecifier] -> Maybe Cpp.ClassBody -> Cpp.Declaration)
cppClassDeclaration = def "cppClassDeclaration" $
  lambda "name" $ lambda "baseSpecs" $ lambda "mbody" $
    inject Cpp._Declaration Cpp._Declaration_class $
      record Cpp._ClassDeclaration [
        Cpp._ClassDeclaration_specifier>>:
          record Cpp._ClassSpecifier [
            Cpp._ClassSpecifier_key>>: inject Cpp._ClassKey Cpp._ClassKey_class unit,
            Cpp._ClassSpecifier_name>>: var "name",
            Cpp._ClassSpecifier_inheritance>>: var "baseSpecs"],
        Cpp._ClassDeclaration_body>>: var "mbody"]

-- | Create an enum class declaration
cppEnumDeclaration :: TypedTermDefinition (String -> Maybe Cpp.ClassBody -> Cpp.Declaration)
cppEnumDeclaration = def "cppEnumDeclaration" $
  lambda "name" $ lambda "mbody" $
    inject Cpp._Declaration Cpp._Declaration_class $
      record Cpp._ClassDeclaration [
        Cpp._ClassDeclaration_specifier>>:
          record Cpp._ClassSpecifier [
            Cpp._ClassSpecifier_key>>: inject Cpp._ClassKey Cpp._ClassKey_enumClass unit,
            Cpp._ClassSpecifier_name>>: var "name",
            Cpp._ClassSpecifier_inheritance>>: list ([] :: [TypedTerm Cpp.BaseSpecifier])],
        Cpp._ClassDeclaration_body>>: var "mbody"]

-- | Create an enum forward declaration (no body)
cppEnumForwardDeclaration :: TypedTermDefinition (String -> Cpp.Declaration)
cppEnumForwardDeclaration = def "cppEnumForwardDeclaration" $
  lambda "name" $
    cppEnumDeclaration @@ var "name" @@ nothing

-- | Convert a PostfixExpression to a full Expression
cppPostfixExpressionToCppExpression :: TypedTermDefinition (Cpp.PostfixExpression -> Cpp.Expression)
cppPostfixExpressionToCppExpression = def "cppPostfixExpressionToCppExpression" $
  lambda "pe" $
    cppUnaryExpressionToCppExpression @@
      (inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix (var "pe"))

-- | Convert a PrimaryExpression to a full Expression
cppPrimaryExpressionToCppExpression :: TypedTermDefinition (Cpp.PrimaryExpression -> Cpp.Expression)
cppPrimaryExpressionToCppExpression = def "cppPrimaryExpressionToCppExpression" $
  lambda "prim" $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary (var "prim"))

-- | Convert a UnaryExpression to a full Expression
cppUnaryExpressionToCppExpression :: TypedTermDefinition (Cpp.UnaryExpression -> Cpp.Expression)
cppUnaryExpressionToCppExpression = def "cppUnaryExpressionToCppExpression" $
  lambda "ue" $
    inject Cpp._Expression Cpp._Expression_assignment $
      inject Cpp._AssignmentExpression Cpp._AssignmentExpression_conditional $
        inject Cpp._ConditionalExpression Cpp._ConditionalExpression_logicalOr $
          cppUnaryExpressionToCppLogicalOrExpression @@ var "ue"

-- | Convert a UnaryExpression to a LogicalOrExpression
cppUnaryExpressionToCppLogicalOrExpression :: TypedTermDefinition (Cpp.UnaryExpression -> Cpp.LogicalOrExpression)
cppUnaryExpressionToCppLogicalOrExpression = def "cppUnaryExpressionToCppLogicalOrExpression" $
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
                      inject Cpp._MultiplicativeExpression Cpp._MultiplicativeExpression_unary (var "ue")


-- ============================================================================
-- Naming functions
-- ============================================================================

-- | Create the accept() method implementation using dynamic_cast chain
createAcceptImplementation :: TypedTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createAcceptImplementation = def "createAcceptImplementation" $
  lambda "tname" $ lambda "variants" $
    inject Cpp._Declaration Cpp._Declaration_template $
      record Cpp._TemplateDeclaration [
        Cpp._TemplateDeclaration_inline>>: boolean False,
        Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
        Cpp._TemplateDeclaration_declaration>>:
          inject Cpp._Declaration Cpp._Declaration_function $
            record Cpp._FunctionDeclaration [
              Cpp._FunctionDeclaration_prefixSpecifiers>>: list ([] :: [TypedTerm Cpp.FunctionSpecifierPrefix]),
              Cpp._FunctionDeclaration_returnType>>:
                inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                  inject Cpp._BasicType Cpp._BasicType_named (string "R"),
              Cpp._FunctionDeclaration_name>>: (className @@ var "tname") ++ string "::accept",
              Cpp._FunctionDeclaration_parameters>>: list [
                record Cpp._Parameter [
                  Cpp._Parameter_type>>:
                    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                      record Cpp._QualifiedType [
                        Cpp._QualifiedType_baseType>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (
                              (visitorName @@ var "tname") ++ string "<R>"),
                        Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
                  Cpp._Parameter_name>>: string "visitor",
                  Cpp._Parameter_unnamed>>: boolean False,
                  Cpp._Parameter_defaultValue>>: nothing]],
              Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_const unit],
              Cpp._FunctionDeclaration_body>>:
                inject Cpp._FunctionBody Cpp._FunctionBody_compound $
                  wrap Cpp._CompoundStatement (
                    -- Generate dynamic_cast if-else chain for each variant
                    -- Each variant checks dynamic_cast and calls visitor.visit(*ptr)
                    -- The last branch throws std::runtime_error with typeid name
                    Lists.map (lambda "ft" $
                      "fname" <~ Core.fieldTypeName (var "ft") $
                      -- Create an if statement with dynamic_cast and visitor.visit call
                      inject Cpp._Statement Cpp._Statement_selection $
                        record Cpp._SelectionStatement [
                          Cpp._SelectionStatement_condition>>:
                            -- auto ptr = dynamic_cast<const VariantName*>(this)
                            inject Cpp._Expression Cpp._Expression_assignment $
                              inject Cpp._AssignmentExpression Cpp._AssignmentExpression_assignment $
                                record Cpp._ExplicitAssignment [
                                  Cpp._ExplicitAssignment_left>>:
                                    cppUnaryExpressionToCppLogicalOrExpression @@
                                      (inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix $
                                        inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                          inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "auto ptr")),
                                  Cpp._ExplicitAssignment_op>>:
                                    inject Cpp._AssignmentOperator Cpp._AssignmentOperator_assign unit,
                                  Cpp._ExplicitAssignment_right>>:
                                    inject Cpp._AssignmentExpression Cpp._AssignmentExpression_conditional $
                                      inject Cpp._ConditionalExpression Cpp._ConditionalExpression_logicalOr $
                                        cppUnaryExpressionToCppLogicalOrExpression @@
                                          (inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix $
                                            inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
                                              record Cpp._FunctionCallOperation [
                                                Cpp._FunctionCallOperation_function>>:
                                                  inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                                    inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (
                                                      string "dynamic_cast<const " ++ (variantName @@ var "tname" @@ var "fname") ++ string "*>"),
                                                Cpp._FunctionCallOperation_arguments>>: list [
                                                  cppPrimaryExpressionToCppExpression @@
                                                    (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "this"))]])],
                          Cpp._SelectionStatement_thenBranch>>:
                            inject Cpp._Statement Cpp._Statement_compound $
                              wrap Cpp._CompoundStatement (list [
                                inject Cpp._Statement Cpp._Statement_jump $
                                  inject Cpp._JumpStatement Cpp._JumpStatement_returnValue $
                                    cppPostfixExpressionToCppExpression @@
                                      (inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
                                        record Cpp._FunctionCallOperation [
                                          Cpp._FunctionCallOperation_function>>:
                                            inject Cpp._PostfixExpression Cpp._PostfixExpression_memberAccess $
                                              record Cpp._MemberAccessOperation [
                                                Cpp._MemberAccessOperation_object>>:
                                                  inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                                    inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "visitor"),
                                                Cpp._MemberAccessOperation_member>>: string "visit"],
                                          Cpp._FunctionCallOperation_arguments>>: list [
                                            cppUnaryExpressionToCppExpression @@
                                              (inject Cpp._UnaryExpression Cpp._UnaryExpression_unaryOp $
                                                record Cpp._UnaryOperation [
                                                  Cpp._UnaryOperation_operator>>:
                                                    inject Cpp._UnaryOperator Cpp._UnaryOperator_dereference unit,
                                                  Cpp._UnaryOperation_operand>>:
                                                    inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix $
                                                      inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                                        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "ptr")])]])]),
                          Cpp._SelectionStatement_elseBranch>>:
                            just (createThrowStmt @@ string "std::runtime_error" @@ (asTerm createTypeIdNameCall))])
                    (var "variants"))]]

-- | Create function body: default if no params, empty compound otherwise
createConstructorBody :: TypedTermDefinition ([Cpp.Parameter] -> Cpp.FunctionBody)
createConstructorBody = def "createConstructorBody" $
  lambda "params" $
    Logic.ifElse
      (Lists.null (var "params"))
      (inject Cpp._FunctionBody Cpp._FunctionBody_default unit)
      (inject Cpp._FunctionBody Cpp._FunctionBody_compound $
        wrap Cpp._CompoundStatement (list ([] :: [TypedTerm Cpp.Statement])))

-- | Create a function call expression
createFunctionCallExpr :: TypedTermDefinition (String -> [Cpp.Expression] -> Cpp.Expression)
createFunctionCallExpr = def "createFunctionCallExpr" $
  lambda "funcName" $ lambda "args" $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
        record Cpp._FunctionCallOperation [
          Cpp._FunctionCallOperation_function>>:
            inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
              inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (var "funcName"),
          Cpp._FunctionCallOperation_arguments>>: var "args"])

-- | Create a header file with pragma once, includes, and declarations
createHeaderFile :: TypedTermDefinition ([Cpp.IncludeDirective] -> [Cpp.Declaration] -> Cpp.Program)
createHeaderFile = def "createHeaderFile" $
  lambda "includes" $ lambda "decls" $
    record Cpp._Program [
      Cpp._Program_preprocessorDirectives>>: list [
        inject Cpp._PreprocessorDirective Cpp._PreprocessorDirective_pragma $
          record Cpp._PragmaDirective [Cpp._PragmaDirective_content>>: string "once"]],
      Cpp._Program_includes>>: var "includes",
      Cpp._Program_declarations>>: var "decls"]

-- | Create an identifier expression
createIdentifierExpr :: TypedTermDefinition (String -> Cpp.Expression)
createIdentifierExpr = def "createIdentifierExpr" $
  lambda "name" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (var "name"))

-- | Create a less-than operator for a record type (trivial/placeholder implementation)
createLessThanOperator :: TypedTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createLessThanOperator = def "createLessThanOperator" $
  lambda "typeName" $ lambda "fields" $
    inject Cpp._Declaration Cpp._Declaration_function $
      record Cpp._FunctionDeclaration [
        Cpp._FunctionDeclaration_prefixSpecifiers>>: list [
          inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_inline unit],
        Cpp._FunctionDeclaration_returnType>>:
          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
            inject Cpp._BasicType Cpp._BasicType_bool unit,
        Cpp._FunctionDeclaration_name>>: string "operator<",
        Cpp._FunctionDeclaration_parameters>>: list [
          unnamedParameter @@ string "lhs" @@
            (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
              inject Cpp._BasicType Cpp._BasicType_named (className @@ var "typeName")),
          unnamedParameter @@ string "rhs" @@
            (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
              inject Cpp._BasicType Cpp._BasicType_named (className @@ var "typeName"))],
        Cpp._FunctionDeclaration_suffixSpecifiers>>: list ([] :: [TypedTerm Cpp.FunctionSpecifierSuffix]),
        Cpp._FunctionDeclaration_body>>:
          inject Cpp._FunctionBody Cpp._FunctionBody_compound $
            wrap Cpp._CompoundStatement (list [
              inject Cpp._Statement Cpp._Statement_jump $
                inject Cpp._JumpStatement Cpp._JumpStatement_returnValue $
                  createLiteralBoolExpr @@ boolean False])]


-- ============================================================================
-- File generation
-- ============================================================================

-- | Create a boolean literal expression
createLiteralBoolExpr :: TypedTermDefinition (Bool -> Cpp.Expression)
createLiteralBoolExpr = def "createLiteralBoolExpr" $
  lambda "val" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_boolean $
          wrap Cpp._BooleanLiteral (var "val"))

-- | Create an integer literal expression
createLiteralIntExpr :: TypedTermDefinition (Int -> Cpp.Expression)
createLiteralIntExpr = def "createLiteralIntExpr" $
  lambda "val" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_integer $
          inject Cpp._IntegerLiteral Cpp._IntegerLiteral_decimal (var "val"))

-- | Create a partial visitor interface with default visit methods that delegate to otherwise()
createPartialVisitorInterface :: TypedTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createPartialVisitorInterface = def "createPartialVisitorInterface" $
  lambda "tname" $ lambda "variants" $
    inject Cpp._Declaration Cpp._Declaration_template $
      record Cpp._TemplateDeclaration [
        Cpp._TemplateDeclaration_inline>>: boolean False,
        Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
        Cpp._TemplateDeclaration_declaration>>:
          cppClassDeclaration @@ (partialVisitorName @@ var "tname")
            @@ list [record Cpp._BaseSpecifier [
                 Cpp._BaseSpecifier_access>>: inject Cpp._AccessSpecifier Cpp._AccessSpecifier_public unit,
                 Cpp._BaseSpecifier_name>>: (visitorName @@ var "tname") ++ string "<R>"]]
            @@ (just (wrap Cpp._ClassBody (
              Lists.concat (list [
                list [asTerm memberSpecificationPublic],
                -- otherwise() pure virtual method
                list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                  inject Cpp._MemberDeclaration Cpp._MemberDeclaration_function $
                    record Cpp._FunctionDeclaration [
                      Cpp._FunctionDeclaration_prefixSpecifiers>>: list [
                        inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
                      Cpp._FunctionDeclaration_returnType>>:
                        inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                          inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                      Cpp._FunctionDeclaration_name>>: string "otherwise",
                      Cpp._FunctionDeclaration_parameters>>: list [
                        constParameter @@ string "value" @@
                          (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (className @@ var "tname"))],
                      Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                        inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_const unit],
                      Cpp._FunctionDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_pure unit]],
                -- Default visit methods delegating to otherwise
                Lists.map (lambda "ft" $
                  "fname" <~ Core.fieldTypeName (var "ft") $
                  inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                    inject Cpp._MemberDeclaration Cpp._MemberDeclaration_function $
                      record Cpp._FunctionDeclaration [
                        Cpp._FunctionDeclaration_prefixSpecifiers>>: list ([] :: [TypedTerm Cpp.FunctionSpecifierPrefix]),
                        Cpp._FunctionDeclaration_returnType>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                        Cpp._FunctionDeclaration_name>>: string "visit",
                        Cpp._FunctionDeclaration_parameters>>: list [
                          constParameter @@ string "value" @@
                            (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                              inject Cpp._BasicType Cpp._BasicType_named (variantName @@ var "tname" @@ var "fname"))],
                        Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                          inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_override unit],
                        Cpp._FunctionDeclaration_body>>:
                          inject Cpp._FunctionBody Cpp._FunctionBody_compound $
                            wrap Cpp._CompoundStatement (list [
                              inject Cpp._Statement Cpp._Statement_jump $
                                inject Cpp._JumpStatement Cpp._JumpStatement_returnValue $
                                  createFunctionCallExpr @@ string "otherwise" @@ list [
                                    createIdentifierExpr @@ string "value"]])])
                (var "variants")]))))]


-- | Create a template type (e.g., std::vector<T>)
createTemplateType :: TypedTermDefinition (String -> [Cpp.TypeExpression] -> Cpp.TypeExpression)
createTemplateType = def "createTemplateType" $
  lambda "name" $ lambda "args" $
    inject Cpp._TypeExpression Cpp._TypeExpression_template $
      record Cpp._TemplateType [
        Cpp._TemplateType_name>>: var "name",
        Cpp._TemplateType_arguments>>:
          Lists.map (lambda "a" $
            inject Cpp._TemplateArgument Cpp._TemplateArgument_type (var "a"))
          (var "args")]

-- | Create a throw statement
createThrowStmt :: TypedTermDefinition (String -> Cpp.Expression -> Cpp.Statement)
createThrowStmt = def "createThrowStmt" $
  lambda "exceptionType" $ lambda "arg" $
    inject Cpp._Statement Cpp._Statement_jump $
      inject Cpp._JumpStatement Cpp._JumpStatement_throw $
        createFunctionCallExpr @@ var "exceptionType" @@ list [var "arg"]


-- ============================================================================
-- Expression chain helpers
-- ============================================================================

-- | Create a typeid(*this).name() call expression
createTypeIdNameCall :: TypedTermDefinition Cpp.Expression
createTypeIdNameCall = def "createTypeIdNameCall" $
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
                        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "typeid"),
                    Cpp._FunctionCallOperation_arguments>>: list [
                      cppPrimaryExpressionToCppExpression @@
                        (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_parenthesized $
                          cppPostfixExpressionToCppExpression @@
                            (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                              inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "*this")))]],
              Cpp._MemberAccessOperation_member>>: string "name"],
        Cpp._FunctionCallOperation_arguments>>: list ([] :: [TypedTerm Cpp.Expression])])

-- | Create a type reference, optionally as a shared_ptr for struct types
createTypeReference :: TypedTermDefinition (Bool -> Name -> Cpp.TypeExpression)
createTypeReference = def "createTypeReference" $
  lambda "isPointer" $ lambda "name" $
    Logic.ifElse (var "isPointer")
      (createTemplateType @@ string "std::shared_ptr" @@
        list [toConstType @@ (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
          inject Cpp._BasicType Cpp._BasicType_named (sanitizeCppName @@ (Names.localNameOf @@ var "name")))])
      (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
        inject Cpp._BasicType Cpp._BasicType_named (sanitizeCppName @@ (Names.localNameOf @@ var "name")))


-- ============================================================================
-- Type encoding
-- ============================================================================

-- | Create the union base class with protected constructor, virtual destructor, and accept method
createUnionBaseClass :: TypedTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createUnionBaseClass = def "createUnionBaseClass" $
  lambda "name" $ lambda "variants" $
    cppClassDeclaration @@ (className @@ var "name") @@ list ([] :: [TypedTerm Cpp.BaseSpecifier]) @@
      (just (wrap Cpp._ClassBody (list [
        asTerm memberSpecificationProtected,
        -- Protected constructor
        inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_constructor $
            record Cpp._ConstructorDeclaration [
              Cpp._ConstructorDeclaration_name>>: className @@ var "name",
              Cpp._ConstructorDeclaration_parameters>>: list ([] :: [TypedTerm Cpp.Parameter]),
              Cpp._ConstructorDeclaration_initializers>>: list ([] :: [TypedTerm Cpp.MemInitializer]),
              Cpp._ConstructorDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_default unit],
        asTerm memberSpecificationPublic,
        -- Virtual destructor
        inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_destructor $
            record Cpp._DestructorDeclaration [
              Cpp._DestructorDeclaration_prefixSpecifiers>>: list [
                inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
              Cpp._DestructorDeclaration_name>>: className @@ var "name",
              Cpp._DestructorDeclaration_suffixSpecifiers>>: list ([] :: [TypedTerm Cpp.FunctionSpecifierSuffix]),
              Cpp._DestructorDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_default unit],
        -- Template accept method
        inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_template $
            record Cpp._TemplateDeclaration [
              Cpp._TemplateDeclaration_inline>>: boolean False,
              Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
              Cpp._TemplateDeclaration_declaration>>:
                inject Cpp._Declaration Cpp._Declaration_function $
                  record Cpp._FunctionDeclaration [
                    Cpp._FunctionDeclaration_prefixSpecifiers>>: list ([] :: [TypedTerm Cpp.FunctionSpecifierPrefix]),
                    Cpp._FunctionDeclaration_returnType>>:
                      inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                        inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                    Cpp._FunctionDeclaration_name>>: string "accept",
                    Cpp._FunctionDeclaration_parameters>>: list [
                      record Cpp._Parameter [
                        Cpp._Parameter_type>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                            record Cpp._QualifiedType [
                              Cpp._QualifiedType_baseType>>:
                                inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                                  inject Cpp._BasicType Cpp._BasicType_named (
                                    (visitorName @@ var "name") ++ string "<R>"),
                              Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
                        Cpp._Parameter_name>>: string "visitor",
                        Cpp._Parameter_unnamed>>: boolean False,
                        Cpp._Parameter_defaultValue>>: nothing]],
                    Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                      inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_const unit],
                    Cpp._FunctionDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_declaration unit]]])))

-- | Create a variant subclass (one branch of a union type)
createVariantClass :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Name -> FieldType -> Either Error Cpp.Declaration)
createVariantClass = def "createVariantClass" $
  "cx" ~> "g" ~> lambda "tname" $ lambda "parentClass" $ lambda "ft" $
    "fname" <~ Core.fieldTypeName (var "ft") $
    "variantType" <~ Core.fieldTypeType (var "ft") $
    "hasValue" <~ Logic.not (Predicates.isUnitType @@ var "variantType") $
    "valueField" <~ Logic.ifElse (var "hasValue")
      (Eithers.map (lambda "cppType" $
        list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_variable $
            record Cpp._VariableDeclaration [
              Cpp._VariableDeclaration_type>>: just (var "cppType"),
              Cpp._VariableDeclaration_name>>: string "value",
              Cpp._VariableDeclaration_initializer>>: nothing,
              Cpp._VariableDeclaration_isAuto>>: boolean False]])
        (encodeType @@ var "cx" @@ var "g" @@ (Strip.deannotateType @@ var "variantType")))
      (right (list ([] :: [TypedTerm Cpp.MemberSpecification]))) $
    "constructorParams" <~ Logic.ifElse (var "hasValue")
      (Eithers.map (lambda "paramType" $
        list [record Cpp._Parameter [
          Cpp._Parameter_type>>: var "paramType",
          Cpp._Parameter_name>>: string "value",
          Cpp._Parameter_unnamed>>: boolean False,
          Cpp._Parameter_defaultValue>>: nothing]])
        (encodeType @@ var "cx" @@ var "g" @@ (Strip.deannotateType @@ var "variantType")))
      (right (list ([] :: [TypedTerm Cpp.Parameter]))) $
    "vFields" <<~ (var "valueField") $
    "vParams" <<~ (var "constructorParams") $
    "initList" <~ Logic.ifElse (var "hasValue")
      (list [record Cpp._MemInitializer [
        Cpp._MemInitializer_name>>: string "value",
        Cpp._MemInitializer_arguments>>: list [createIdentifierExpr @@ string "value"]]])
      (list ([] :: [TypedTerm Cpp.MemInitializer])) $
      right (cppClassDeclaration
        @@ (variantName @@ var "tname" @@ var "fname")
        @@ list [record Cpp._BaseSpecifier [
             Cpp._BaseSpecifier_access>>: inject Cpp._AccessSpecifier Cpp._AccessSpecifier_public unit,
             Cpp._BaseSpecifier_name>>: className @@ var "parentClass"]]
        @@ (just (wrap Cpp._ClassBody (
          Lists.concat (list [
            list [asTerm memberSpecificationPublic],
            var "vFields",
            list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
              inject Cpp._MemberDeclaration Cpp._MemberDeclaration_constructor $
                record Cpp._ConstructorDeclaration [
                  Cpp._ConstructorDeclaration_name>>: variantName @@ var "tname" @@ var "fname",
                  Cpp._ConstructorDeclaration_parameters>>: var "vParams",
                  Cpp._ConstructorDeclaration_initializers>>: var "initList",
                  Cpp._ConstructorDeclaration_body>>: createConstructorBody @@ var "vParams"]]])))))

-- | Create a visitor interface with pure virtual visit methods
createVisitorInterface :: TypedTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createVisitorInterface = def "createVisitorInterface" $
  lambda "tname" $ lambda "variants" $
    inject Cpp._Declaration Cpp._Declaration_template $
      record Cpp._TemplateDeclaration [
        Cpp._TemplateDeclaration_inline>>: boolean False,
        Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
        Cpp._TemplateDeclaration_declaration>>:
          cppClassDeclaration @@ (visitorName @@ var "tname") @@ list ([] :: [TypedTerm Cpp.BaseSpecifier]) @@
            (just (wrap Cpp._ClassBody (
              Lists.concat (list [
                list [asTerm memberSpecificationPublic],
                Lists.map (lambda "ft" $
                  "fname" <~ Core.fieldTypeName (var "ft") $
                  inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                    inject Cpp._MemberDeclaration Cpp._MemberDeclaration_function $
                      record Cpp._FunctionDeclaration [
                        Cpp._FunctionDeclaration_prefixSpecifiers>>: list [
                          inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
                        Cpp._FunctionDeclaration_returnType>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                        Cpp._FunctionDeclaration_name>>: string "visit",
                        Cpp._FunctionDeclaration_parameters>>: list [
                          record Cpp._Parameter [
                            Cpp._Parameter_type>>:
                              inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                                record Cpp._QualifiedType [
                                  Cpp._QualifiedType_baseType>>:
                                    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                                      record Cpp._QualifiedType [
                                        Cpp._QualifiedType_baseType>>:
                                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                                            inject Cpp._BasicType Cpp._BasicType_named (variantName @@ var "tname" @@ var "fname"),
                                        Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit],
                                  Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
                            Cpp._Parameter_name>>: string "value",
                            Cpp._Parameter_unnamed>>: boolean False,
                            Cpp._Parameter_defaultValue>>: nothing]],
                        Cpp._FunctionDeclaration_suffixSpecifiers>>: list ([] :: [TypedTerm Cpp.FunctionSpecifierSuffix]),
                        Cpp._FunctionDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_pure unit])
                (var "variants"),
                list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                  inject Cpp._MemberDeclaration Cpp._MemberDeclaration_destructor $
                    record Cpp._DestructorDeclaration [
                      Cpp._DestructorDeclaration_prefixSpecifiers>>: list [
                        inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
                      Cpp._DestructorDeclaration_name>>: visitorName @@ var "tname",
                      Cpp._DestructorDeclaration_suffixSpecifiers>>: list ([] :: [TypedTerm Cpp.FunctionSpecifierSuffix]),
                      Cpp._DestructorDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_default unit]]]))))]

def :: String -> TypedTerm a -> TypedTermDefinition a
def = definitionInModule module_


-- | An empty list term, avoiding ambiguous type variable issues with 'list []'
emptyList :: TypedTerm [a]
emptyList = TypedTerm $ TermList []

-- | Encode a type application (template instantiation)
encodeApplicationType :: TypedTermDefinition (InferenceContext -> Graph -> ApplicationType -> Either Error Cpp.TypeExpression)
encodeApplicationType = def "encodeApplicationType" $
  "cx" ~> "g" ~> lambda "at" $
    "body" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeFunction (var "at")) $
    "arg" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeArgument (var "at")) $
      right (createTemplateType @@ string "TODO_template" @@ list [var "body", var "arg"])

-- | Encode an enum type as a C++ enum class
encodeEnumType :: TypedTermDefinition (InferenceContext -> Graph -> Name -> [FieldType] -> Maybe String -> Either Error [Cpp.Declaration])
encodeEnumType = def "encodeEnumType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "tfields" $ lambda "comment" $
    right (list [
      cppEnumDeclaration @@ (className @@ var "name") @@
        (just (wrap Cpp._ClassBody (
          Lists.map (lambda "ft" $
            inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
              inject Cpp._MemberDeclaration Cpp._MemberDeclaration_variable $
                record Cpp._VariableDeclaration [
                  Cpp._VariableDeclaration_type>>: nothing,
                  Cpp._VariableDeclaration_name>>: encodeEnumValue @@ Core.fieldTypeName (var "ft"),
                  Cpp._VariableDeclaration_initializer>>: nothing,
                  Cpp._VariableDeclaration_isAuto>>: boolean False])
          (var "tfields"))))])

-- | Encode an enum value in UPPER_SNAKE_CASE
encodeEnumValue :: TypedTermDefinition (Name -> String)
encodeEnumValue = def "encodeEnumValue" $
  lambda "fname" $
    sanitizeCppName @@ (Formatting.convertCaseCamelToUpperSnake @@ Core.unName (var "fname"))

-- | Encode a field name in lower_snake_case
encodeFieldName :: TypedTermDefinition (Name -> String)
encodeFieldName = def "encodeFieldName" $
  lambda "fname" $
    sanitizeCppName @@ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (var "fname"))

-- | Encode a field type as a VariableDeclaration
encodeFieldType :: TypedTermDefinition (Bool -> FieldType -> InferenceContext -> Graph -> Either Error Cpp.VariableDeclaration)
encodeFieldType = def "encodeFieldType" $
  lambda "isParameter" $ lambda "ft" $ "cx" ~> lambda "g" $
    "fname" <~ Core.fieldTypeName (var "ft") $
    "ftype" <~ Core.fieldTypeType (var "ft") $
    "cppType" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftype") $
      right (record Cpp._VariableDeclaration [
        Cpp._VariableDeclaration_type>>: just (var "cppType"),
        Cpp._VariableDeclaration_name>>: encodeFieldName @@ var "fname",
        Cpp._VariableDeclaration_initializer>>: nothing,
        Cpp._VariableDeclaration_isAuto>>: boolean False])

-- | Encode a forall type (strip the quantifier)
encodeForallType :: TypedTermDefinition (InferenceContext -> Graph -> ForallType -> Either Error Cpp.TypeExpression)
encodeForallType = def "encodeForallType" $
  "cx" ~> "g" ~> lambda "lt" $
    encodeType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "lt")

-- | Encode a function type as std::function<R(Args...)>
encodeFunctionType :: TypedTermDefinition (InferenceContext -> Graph -> FunctionType -> Either Error Cpp.TypeExpression)
encodeFunctionType = def "encodeFunctionType" $
  "cx" ~> "g" ~> lambda "ft" $
    "dom" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeDomain (var "ft")) $
    "cod" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft")) $
      right (inject Cpp._TypeExpression Cpp._TypeExpression_function $
        record Cpp._FunctionType [
          Cpp._FunctionType_returnType>>: var "cod",
          Cpp._FunctionType_parameters>>: list [
            record Cpp._Parameter [
              Cpp._Parameter_type>>: var "dom",
              Cpp._Parameter_name>>: string "",
              Cpp._Parameter_unnamed>>: boolean False,
              Cpp._Parameter_defaultValue>>: nothing]]])

-- | Encode a literal type as a C++ type expression
encodeLiteralType :: TypedTermDefinition (LiteralType -> Cpp.TypeExpression)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $
    inject Cpp._TypeExpression Cpp._TypeExpression_basic $
      cases _LiteralType (var "lt") Nothing [
        _LiteralType_binary>>: constant $
          inject Cpp._BasicType Cpp._BasicType_char unit,
        _LiteralType_boolean>>: constant $
          inject Cpp._BasicType Cpp._BasicType_bool unit,
        _LiteralType_float>>: lambda "ft" $
          cases _FloatType (var "ft") (Just $ inject Cpp._BasicType Cpp._BasicType_double unit) [
            _FloatType_float32>>: constant $ inject Cpp._BasicType Cpp._BasicType_float unit,
            _FloatType_float64>>: constant $ inject Cpp._BasicType Cpp._BasicType_double unit],
        _LiteralType_integer>>: lambda "it" $
          cases _IntegerType (var "it") (Just $ inject Cpp._BasicType Cpp._BasicType_int unit) [
            _IntegerType_bigint>>: constant $ inject Cpp._BasicType Cpp._BasicType_int unit,
            _IntegerType_int8>>: constant $ inject Cpp._BasicType Cpp._BasicType_char unit,
            _IntegerType_int16>>: constant $ inject Cpp._BasicType Cpp._BasicType_named (string "int16_t"),
            _IntegerType_int32>>: constant $ inject Cpp._BasicType Cpp._BasicType_int unit,
            _IntegerType_int64>>: constant $ inject Cpp._BasicType Cpp._BasicType_named (string "int64_t")],
        _LiteralType_string>>: constant $
          inject Cpp._BasicType Cpp._BasicType_string unit]

-- | Encode a name with specified convention and optional qualification
encodeName :: TypedTermDefinition (Bool -> CaseConvention -> x -> Name -> String)
encodeName = def "encodeName" $
  doc "Encode a name with a specified case convention, optionally qualified" $
  lambda "isQualified" $ lambda "conv" $ lambda "env" $ lambda "name" $
    -- Simplified: use local name with sanitization
    sanitizeCppName @@ (Names.localNameOf @@ var "name")

-- | Encode a namespace as a C++ namespace string (e.g., "hydra.cpp" -> "hydra::ext::cpp")
encodeNamespace :: TypedTermDefinition (ModuleName -> String)
encodeNamespace = def "encodeNamespace" $
  lambda "ns" $
    Strings.intercalate (string "::")
      (Lists.map
        (lambda "seg" $ Formatting.convertCaseCamelToLowerSnake @@ var "seg")
        (Strings.splitOn (string ".") (Packaging.unModuleName (var "ns"))))

-- | Encode a record type as a C++ class with fields and constructor
encodeRecordType :: TypedTermDefinition (InferenceContext -> Graph -> Name -> [FieldType] -> Maybe String -> Either Error [Cpp.Declaration])
encodeRecordType = def "encodeRecordType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "rt" $ lambda "comment" $
    "cppFields" <<~ (Eithers.mapList (lambda "f" $ encodeFieldType @@ boolean False @@ var "f" @@ var "cx" @@ var "g") (var "rt")) $
    "constructorParams" <<~ (Eithers.mapList (lambda "f" $ encodeFieldType @@ boolean True @@ var "f" @@ var "cx" @@ var "g") (var "rt")) $
      let classDecl = cppClassDeclaration @@ (className @@ var "name") @@ list ([] :: [TypedTerm Cpp.BaseSpecifier]) @@
            (just (wrap Cpp._ClassBody (
              Lists.concat (list [
                list [asTerm memberSpecificationPublic],
                Lists.map (lambda "field" $
                  inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                    inject Cpp._MemberDeclaration Cpp._MemberDeclaration_variable (var "field"))
                  (var "cppFields"),
                list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                  inject Cpp._MemberDeclaration Cpp._MemberDeclaration_constructor $
                    record Cpp._ConstructorDeclaration [
                      Cpp._ConstructorDeclaration_name>>: className @@ var "name",
                      Cpp._ConstructorDeclaration_parameters>>:
                        Lists.map (lambda "p" $
                          record Cpp._Parameter [
                            Cpp._Parameter_type>>: Optionals.fromOptional
                              (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                                inject Cpp._BasicType Cpp._BasicType_int unit)
                              ((project Cpp._VariableDeclaration Cpp._VariableDeclaration_type @@ var "p")),
                            Cpp._Parameter_name>>: (project Cpp._VariableDeclaration Cpp._VariableDeclaration_name @@ var "p"),
                            Cpp._Parameter_unnamed>>: boolean False,
                            Cpp._Parameter_defaultValue>>: nothing])
                          (var "constructorParams"),
                      Cpp._ConstructorDeclaration_initializers>>:
                        Lists.map (lambda "field" $
                          record Cpp._MemInitializer [
                            Cpp._MemInitializer_name>>: (project Cpp._VariableDeclaration Cpp._VariableDeclaration_name @@ var "field"),
                            Cpp._MemInitializer_arguments>>: list [
                              createIdentifierExpr @@ (project Cpp._VariableDeclaration Cpp._VariableDeclaration_name @@ var "field")]])
                          (var "cppFields"),
                      Cpp._ConstructorDeclaration_body>>: createConstructorBody @@ var "constructorParams"]]]
              ))))
          ltOp = createLessThanOperator @@ var "name" @@ var "rt"
      in right (list [classDecl, ltOp])

-- | Encode a Hydra type as a C++ type expression
encodeType :: TypedTermDefinition (InferenceContext -> Graph -> Type -> Either Error Cpp.TypeExpression)
encodeType = def "encodeType" $
  "cx" ~> "g" ~> lambda "typ" $
    "t" <~ (Strip.deannotateType @@ var "typ") $
    cases _Type (var "t") (Just $
      left (Error.errorOther $ Error.otherError $ string "Unsupported type"))
    [_Type_application>>: lambda "at" $
       encodeApplicationType @@ var "cx" @@ var "g" @@ var "at",
     _Type_either>>: lambda "et" $
       "lt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeLeft (var "et")) $
       "rt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeRight (var "et")) $
         right (toConstType @@ (createTemplateType @@ string "std::variant" @@ list [var "lt", var "rt"])),
     _Type_function>>: lambda "ft" $
       encodeFunctionType @@ var "cx" @@ var "g" @@ var "ft",
     _Type_forall>>: lambda "lt" $
       encodeForallType @@ var "cx" @@ var "g" @@ var "lt",
     _Type_list>>: lambda "et" $
       Eithers.map (lambda "enc" $ toConstType @@ (createTemplateType @@ string "std::vector" @@ list [var "enc"]))
         (encodeType @@ var "cx" @@ var "g" @@ var "et"),
     _Type_map>>: lambda "mt" $
       "kt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeKeys (var "mt")) $
       "vt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeValues (var "mt")) $
         right (toConstType @@ (createTemplateType @@ string "std::map" @@ list [var "kt", var "vt"])),
     _Type_literal>>: lambda "lt" $
       right (encodeLiteralType @@ var "lt"),
     _Type_optional>>: lambda "et" $
       Eithers.map (lambda "enc" $ toConstType @@ (createTemplateType @@ string "std::optional" @@ list [var "enc"]))
         (encodeType @@ var "cx" @@ var "g" @@ var "et"),
     _Type_pair>>: lambda "pt" $
       "ft" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeFirst (var "pt")) $
       "st" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeSecond (var "pt")) $
         right (toConstType @@ (createTemplateType @@ string "std::pair" @@ list [var "ft", var "st"])),
     _Type_record>>: lambda "rt" $
       left (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")),
     _Type_set>>: lambda "et" $
       Eithers.map (lambda "enc" $ toConstType @@ (createTemplateType @@ string "std::set" @@ list [var "enc"]))
         (encodeType @@ var "cx" @@ var "g" @@ var "et"),
     _Type_union>>: lambda "rt" $
       left (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")),
     _Type_variable>>: lambda "name" $
       right (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
         inject Cpp._BasicType Cpp._BasicType_named (sanitizeCppName @@ Core.unName (var "name"))),
     _Type_wrap>>: lambda "wt" $
       left (Error.errorOther $ Error.otherError (string "unexpected anonymous wrapped type")),
     _Type_unit>>: constant $
       right (createTemplateType @@ string "std::tuple" @@ list ([] :: [TypedTerm Cpp.TypeExpression]))]

-- | Encode a type as a typedef / using declaration
encodeTypeAlias :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Type -> Maybe String -> Either Error Cpp.Declaration)
encodeTypeAlias = def "encodeTypeAlias" $
  "cx" ~> "g" ~> lambda "name" $ lambda "typ" $ lambda "comment" $
    "cppType" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
      right (inject Cpp._Declaration Cpp._Declaration_typedef $
        record Cpp._TypedefDeclaration [
          Cpp._TypedefDeclaration_name>>: className @@ var "name",
          Cpp._TypedefDeclaration_type>>: var "cppType",
          Cpp._TypedefDeclaration_isUsing>>: boolean True])

-- | Encode a top-level type definition (dispatches to record/union/wrap)
encodeTypeDefinition :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Type -> Either Error [Cpp.Declaration])
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "name" $ lambda "typ" $
    "t" <~ (Strip.deannotateType @@ var "typ") $
    cases _Type (var "t") (Just $
      left (Error.errorOther $ Error.otherError $ string "unexpected type in definition: " ++ (ShowCore.type_ @@ var "typ")))
    [_Type_forall>>: lambda "fa" $
       encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "name" @@ Core.forallTypeBody (var "fa"),
     _Type_record>>: lambda "rt" $
       encodeRecordType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ nothing,
     _Type_union>>: lambda "rt" $
       encodeUnionType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ nothing,
     _Type_wrap>>: lambda "wt" $
       encodeWrappedType @@ var "cx" @@ var "g" @@ var "name" @@ var "wt" @@ nothing]


-- ============================================================================
-- Struct/union/enum encoding
-- ============================================================================

-- | Encode a union type (dispatches to enum or variant based on content)
encodeUnionType :: TypedTermDefinition (InferenceContext -> Graph -> Name -> [FieldType] -> Maybe String -> Either Error [Cpp.Declaration])
encodeUnionType = def "encodeUnionType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "rt" $ lambda "comment" $
    Logic.ifElse (Predicates.isEnumRowType @@ var "rt")
      (encodeEnumType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ var "comment")
      (encodeVariantType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ var "comment")

-- | Encode a variant (tagged union) type as a class hierarchy with visitor pattern
encodeVariantType :: TypedTermDefinition (InferenceContext -> Graph -> Name -> [FieldType] -> Maybe String -> Either Error [Cpp.Declaration])
encodeVariantType = def "encodeVariantType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "variants" $ lambda "comment" $
    "variantClasses" <<~ (Eithers.mapList
      (lambda "v" $ createVariantClass @@ var "cx" @@ var "g" @@ var "name" @@ var "name" @@ var "v")
      (var "variants")) $
      right (Lists.concat (list [
        generateForwardDeclarations @@ var "name" @@ var "variants",
        list [createVisitorInterface @@ var "name" @@ var "variants"],
        list [createUnionBaseClass @@ var "name" @@ var "variants"],
        var "variantClasses",
        list [createPartialVisitorInterface @@ var "name" @@ var "variants"],
        list [createAcceptImplementation @@ var "name" @@ var "variants"]]))

-- | Encode a wrapped type as a single-field record
encodeWrappedType :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Type -> Maybe String -> Either Error [Cpp.Declaration])
encodeWrappedType = def "encodeWrappedType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "typ" $ lambda "comment" $
    encodeRecordType @@ var "cx" @@ var "g" @@ var "name"
      @@ list [
            record _FieldType [
              _FieldType_name>>: wrap _Name (string "value"),
              _FieldType_type>>: var "typ"]]
      @@ var "comment"


-- ============================================================================
-- Visitor pattern
-- ============================================================================

-- | Find includes for a set of type definitions
findIncludes :: TypedTermDefinition (Bool -> ModuleName -> [TypeDefinition] -> [Cpp.IncludeDirective])
findIncludes = def "findIncludes" $
  lambda "withFwd" $ lambda "ns" $ lambda "defs" $
    -- System includes based on metadata, plus domain includes for cross-namespace dependencies
    -- For simplicity, include the standard set that the staging code conditionally includes
    Lists.concat (list [
      list [
        record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: string "memory",
          Cpp._IncludeDirective_isSystem>>: boolean True],
        record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: string "stdexcept",
          Cpp._IncludeDirective_isSystem>>: boolean True]],
      -- Domain includes from cross-namespace type dependencies
      Lists.map (lambda "depName" $
        record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: bindingNameToFilePath @@ var "depName",
          Cpp._IncludeDirective_isSystem>>: boolean False])
        (findTypeDependencies @@ var "ns" @@ var "defs"),
      -- Fwd include if needed
      Logic.ifElse (var "withFwd")
        (list [record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: bindingNameToFilePath @@ (fwdHeaderName @@ var "ns"),
          Cpp._IncludeDirective_isSystem>>: boolean False]])
        (list ([] :: [TypedTerm Cpp.IncludeDirective]))])

-- | Find type dependencies that are in other namespaces
findTypeDependencies :: TypedTermDefinition (ModuleName -> [TypeDefinition] -> [Name])
findTypeDependencies = def "findTypeDependencies" $
  lambda "ns" $ lambda "defs" $
    Lists.filter
      (lambda "n" $
        Logic.not (Equality.equal
          (Optionals.map (reify Packaging.unModuleName) (Names.moduleNameOf @@ var "n"))
          (just (Packaging.unModuleName (var "ns")))))
      (Sets.toList (Lists.foldl
        (lambda "acc" $ lambda "d" $
          Sets.union (var "acc") (Dependencies.typeDependencyNames @@ boolean True @@ (Core.typeSchemeBody $ Packaging.typeDefinitionBody (var "d"))))
        (Sets.empty)
        (var "defs")))

-- | Construct the forward-declaration header name for a namespace
fwdHeaderName :: TypedTermDefinition (ModuleName -> Name)
fwdHeaderName = def "fwdHeaderName" $
  lambda "ns" $
    Names.unqualifyName @@
      (record _QualifiedName [
        _QualifiedName_moduleName>>: just (var "ns"),
        _QualifiedName_local>>: string "Fwd"])

-- | Gather metadata from definitions (simplified: always include common headers)
gatherMetadata :: TypedTermDefinition ([Definition] -> Bool)
gatherMetadata = def "gatherMetadata" $
  lambda "defs" $ boolean True

-- | Generate forward declarations for all variant subclasses
generateForwardDeclarations :: TypedTermDefinition (Name -> [FieldType] -> [Cpp.Declaration])
generateForwardDeclarations = def "generateForwardDeclarations" $
  lambda "tname" $ lambda "fields" $
    Lists.map (lambda "ft" $
      cppClassDeclaration @@ (variantName @@ var "tname" @@ Core.fieldTypeName (var "ft")) @@ list ([] :: [TypedTerm Cpp.BaseSpecifier]) @@ nothing)
    (var "fields")

-- | Generate a single type header file
generateTypeFile :: TypedTermDefinition (ModuleName -> TypeDefinition -> InferenceContext -> Graph -> Either Error (FilePath, String))
generateTypeFile = def "generateTypeFile" $
  lambda "ns" $ lambda "def_" $ "cx" ~> lambda "g" $
    "name" <~ Packaging.typeDefinitionName (var "def_") $
    "typ" <~ (Core.typeSchemeBody $ Packaging.typeDefinitionBody (var "def_")) $
    "decls" <<~ (encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "name" @@ var "typ") $
    "includes" <~ (findIncludes @@ boolean True @@ var "ns" @@ list [var "def_"]) $
      right (serializeHeaderFile @@ var "name" @@ var "includes"
        @@ list [namespaceDecl @@ var "ns" @@ var "decls"])

-- | Generate all type header files for a module (fwd file + individual class files)
generateTypeFiles :: TypedTermDefinition (ModuleName -> [TypeDefinition] -> InferenceContext -> Graph -> Either Error [(FilePath, String)])
generateTypeFiles = def "generateTypeFiles" $
  lambda "ns" $ lambda "defs" $ "cx" ~> lambda "g" $
    "classFiles" <<~ (Eithers.mapList
      (lambda "d" $ generateTypeFile @@ var "ns" @@ var "d" @@ var "cx" @@ var "g")
      (var "defs")) $
      right (var "classFiles")

-- | Check whether a type maps to an STL container type
isStdContainerType :: TypedTermDefinition (Type -> Bool)
isStdContainerType = def "isStdContainerType" $
  lambda "typ" $
    "t" <~ (Strip.deannotateType @@ var "typ") $
    cases _Type (var "t") (Just $ boolean False)
    [_Type_application>>: lambda "at" $
       isStdContainerType @@ Core.applicationTypeFunction (var "at"),
     _Type_list>>: constant $ boolean True,
     _Type_map>>: constant $ boolean True,
     _Type_optional>>: constant $ boolean True,
     _Type_set>>: constant $ boolean True]

-- | Check whether a type is a struct type (not a literal and not an enum)
isStructType :: TypedTermDefinition (Type -> Bool)
isStructType = def "isStructType" $
  lambda "rawType" $
    "t" <~ (Resolution.fullyStripType @@ var "rawType") $
    "isLiteral" <~ cases _Type (var "t") (Just $ boolean False)
      [_Type_literal>>: constant $ boolean True] $
    Logic.and
      (Logic.not (var "isLiteral"))
      (Logic.not (Predicates.isEnumType @@ var "rawType"))

-- | Check whether a type maps to a C++ template type (string or STL container)
isTemplateType :: TypedTermDefinition (Type -> Bool)
isTemplateType = def "isTemplateType" $
  lambda "typ" $
    "t" <~ (Strip.deannotateType @@ var "typ") $
    Logic.or
      (cases _Type (var "t") (Just $ boolean False)
        [_Type_literal>>: lambda "lt" $
          cases _LiteralType (var "lt") (Just $ boolean False)
            [_LiteralType_string>>: constant $ boolean True]])
      (isStdContainerType @@ var "typ")


-- ============================================================================
-- Module entry point
-- ============================================================================

-- | Protected access label
memberSpecificationProtected :: TypedTermDefinition Cpp.MemberSpecification
memberSpecificationProtected = def "memberSpecificationProtected" $
  inject Cpp._MemberSpecification Cpp._MemberSpecification_accessLabel $
    inject Cpp._AccessSpecifier Cpp._AccessSpecifier_protected unit

-- | Public access label
memberSpecificationPublic :: TypedTermDefinition Cpp.MemberSpecification
memberSpecificationPublic = def "memberSpecificationPublic" $
  inject Cpp._MemberSpecification Cpp._MemberSpecification_accessLabel $
    inject Cpp._AccessSpecifier Cpp._AccessSpecifier_public unit

-- | Convert a module to C++ code files (entry point)
moduleToCpp :: TypedTermDefinition (Module -> [Definition] -> InferenceContext -> Graph -> Either Error (M.Map FilePath String))
moduleToCpp = def "moduleToCpp" $
  lambda "mod" $ lambda "defs" $ "cx" ~> lambda "g" $
    "ns" <~ Packaging.moduleName (var "mod") $
    "typeDefs" <~ Pairs.first (Environment.partitionDefinitions @@ var "defs") $
    "typeFiles" <<~ (generateTypeFiles @@ var "ns" @@ var "typeDefs" @@ var "cx" @@ var "g") $
      right (Maps.fromList (var "typeFiles"))

-- | Create a namespace declaration wrapping declarations
namespaceDecl :: TypedTermDefinition (ModuleName -> [Cpp.Declaration] -> Cpp.Declaration)
namespaceDecl = def "namespaceDecl" $
  lambda "ns" $ lambda "decls" $
    inject Cpp._Declaration Cpp._Declaration_namespace $
      record Cpp._NamespaceDeclaration [
        Cpp._NamespaceDeclaration_name>>: encodeNamespace @@ var "ns",
        Cpp._NamespaceDeclaration_declarations>>: var "decls"]

-- | Construct a partial visitor interface name
partialVisitorName :: TypedTermDefinition (Name -> String)
partialVisitorName = def "partialVisitorName" $
  lambda "name" $
    sanitizeCppName @@ ((Names.localNameOf @@ var "name") ++ string "PartialVisitor")

-- | Sanitize a name to be valid in C++
sanitizeCppName :: TypedTermDefinition (String -> String)
sanitizeCppName = def "sanitizeCppName" $
  lambda "name" $
    Formatting.sanitizeWithUnderscores @@ (asTerm CppLanguageSource.cppReservedWords) @@ var "name"

-- | Serialize a header file from name, includes, and declarations
serializeHeaderFile :: TypedTermDefinition (Name -> [Cpp.IncludeDirective] -> [Cpp.Declaration] -> (FilePath, String))
serializeHeaderFile = def "serializeHeaderFile" $
  lambda "name" $ lambda "includes" $ lambda "decls" $
    pair
      (bindingNameToFilePath @@ var "name")
      (SerializationSource.printExpr @@
        (SerializationSource.parenthesize @@
          (TypedTerm (TermVariable (Name "hydra.cpp.serde.programToExpr"))
            @@ (createHeaderFile @@ var "includes" @@ var "decls"))))

-- | Wrap a type expression with const qualifier
toConstType :: TypedTermDefinition (Cpp.TypeExpression -> Cpp.TypeExpression)
toConstType = def "toConstType" $
  lambda "baseType" $
    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
      record Cpp._QualifiedType [
        Cpp._QualifiedType_baseType>>: var "baseType",
        Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit]

-- | Create an unnamed parameter (for operator overloads, etc.)
unnamedParameter :: TypedTermDefinition (String -> Cpp.TypeExpression -> Cpp.Parameter)
unnamedParameter = def "unnamedParameter" $
  lambda "name" $ lambda "typ" $
    record Cpp._Parameter [
      Cpp._Parameter_type>>: var "typ",
      Cpp._Parameter_name>>: var "name",
      Cpp._Parameter_unnamed>>: boolean True,
      Cpp._Parameter_defaultValue>>: nothing]

-- | Construct a variant class name (e.g., "MyType" + "field" -> "MyTypeField")
variantName :: TypedTermDefinition (Name -> Name -> String)
variantName = def "variantName" $
  lambda "tname" $ lambda "fname" $
    sanitizeCppName @@ (
      (Names.localNameOf @@ var "tname")
      ++ (Formatting.capitalize @@ Core.unName (var "fname")))

-- | Construct a visitor interface name
visitorName :: TypedTermDefinition (Name -> String)
visitorName = def "visitorName" $
  lambda "name" $
    sanitizeCppName @@ ((Names.localNameOf @@ var "name") ++ string "Visitor")
