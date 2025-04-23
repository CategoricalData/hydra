-- Note: this file was created with the help of a large language model. It requires further human review.

{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Cpp.Syntax where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


cppNs = Namespace "hydra.ext.cpp.syntax"
cpp = typeref cppNs

cppSyntaxModule :: Module
cppSyntaxModule = Module cppNs elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A C++ syntax model, focusing on features for representing algebraic data types and declarative computations")
  where
    def = datatype cppNs

    elements = accessSpecifiers ++ declarationTypes ++ expressionTypes ++ statementTypes
               ++ typeTypes ++ literalTypes ++ containers ++ utilities ++ operatorTypes

    accessSpecifiers = [
      def "AccessSpecifier" $ enum ["public", "protected", "private", "none"]]

    -- Declaration-related types
    declarationTypes = [
-- <program> ::= <include-directives> <declarations>
      def "Program" $ record [
        "includes">: list $ cpp "IncludeDirective",
        "declarations">: list $ cpp "Declaration"],

-- <include-directive> ::= "#include" "<" <identifier> ">" | "#include" "\"" <identifier> "\""
      def "IncludeDirective" $ record [
        "name">: string,
        "isSystem">: boolean],

-- <declaration> ::= <class-declaration> | <function-declaration> | <variable-declaration> | <typedef-declaration> | <namespace-declaration>
      def "Declaration" $ union [
        "class">: cpp "ClassDeclaration",
        "function">: cpp "FunctionDeclaration",
        "variable">: cpp "VariableDeclaration",
        "typedef">: cpp "TypedefDeclaration",
        "namespace">: cpp "NamespaceDeclaration"],

-- <namespace-declaration> ::= "namespace" <identifier> "{" <declarations> "}" ";"
      def "NamespaceDeclaration" $ record [
        "name">: string,
        "declarations">: list $ cpp "Declaration"],

-- <typedef-declaration> ::= "typedef" <type-expression> <identifier> ";" | "using" <identifier> "=" <type-expression> ";"
      def "TypedefDeclaration" $ record [
        "name">: string,
        "type">: cpp "TypeExpression",
        "isUsing">: boolean],

-- <class-declaration> ::= <class-specifier> <class-body>
      def "ClassDeclaration" $ record [
        "specifier">: cpp "ClassSpecifier",
        "body">: cpp "ClassBody"],

-- <class-specifier> ::= <class-key> <identifier> <inheritance-list>
      def "ClassSpecifier" $ record [
        "key">: cpp "ClassKey",
        "name">: string,
        "inheritance">: list $ cpp "BaseSpecifier"],

-- <class-key> ::= "class" | "struct"
      def "ClassKey" $ enum ["class", "struct"],

-- <base-specifier> ::= <access-specifier> <identifier>
      def "BaseSpecifier" $ record [
        "access">: cpp "AccessSpecifier",
        "name">: string],

-- <class-body> ::= "{" <member-specifications> "}" ";"
      def "ClassBody" $ list $ cpp "MemberSpecification",

-- <member-specification> ::= <access-specifier> ":" | <member-declaration>
      def "MemberSpecification" $ union [
        "accessLabel">: cpp "AccessSpecifier",
        "member">: cpp "MemberDeclaration"],

-- <member-declaration> ::= <function-declaration> | <variable-declaration> | <constructor-declaration> | <destructor-declaration> | <nested-class-declaration>
      def "MemberDeclaration" $ union [
        "function">: cpp "FunctionDeclaration",
        "variable">: cpp "VariableDeclaration",
        "constructor">: cpp "ConstructorDeclaration",
        "destructor">: cpp "DestructorDeclaration",
        "nestedClass">: cpp "ClassDeclaration"],

-- <constructor-declaration> ::= <identifier> "(" <parameter-list> ")" <constructor-initializers> <function-body>
      def "ConstructorDeclaration" $ record [
        "name">: string,
        "parameters">: list $ cpp "Parameter",
        "initializers">: list $ cpp "MemInitializer",
        "body">: cpp "CompoundStatement"],

-- <mem-initializer> ::= <identifier> "(" <expression-list> ")"
      def "MemInitializer" $ record [
        "name">: string,
        "arguments">: list $ cpp "Expression"],

-- <destructor-declaration> ::= "~" <identifier> "(" ")" <function-body>
      def "DestructorDeclaration" $ record [
        "name">: string,
        "body">: cpp "CompoundStatement"],

-- <function-declaration> ::= <type-expression> <identifier> "(" <parameter-list> ")" <function-specifiers> <function-body>
      def "FunctionDeclaration" $ record [
        "returnType">: cpp "TypeExpression",
        "name">: string,
        "parameters">: list $ cpp "Parameter",
        "specifiers">: list $ cpp "FunctionSpecifier",
        "body">: cpp "FunctionBody"],

-- <function-specifier> ::= "const" | "noexcept" | "override" | "final"
      def "FunctionSpecifier" $ enum ["const", "noexcept", "override", "final", "static"],

-- <parameter> ::= <type-expression> <identifier> <default-arg>
      def "Parameter" $ record [
        "type">: cpp "TypeExpression",
        "name">: string,
        "defaultValue">: optional $ cpp "Expression"],

-- <function-body> ::= <compound-statement> | ";"
      def "FunctionBody" $ union [
        "compound">: cpp "CompoundStatement",
        "declaration">: unit],

-- <variable-declaration> ::= <type-expression> <identifier> <initializer> ";" | "auto" <identifier> <initializer> ";"
      def "VariableDeclaration" $ record [
        "type">: cpp "TypeExpression",
        "name">: string,
        "initializer">: optional $ cpp "Expression",
        "isAuto">: boolean],

-- <variant-declaration> ::= "std::variant" "<" <type-list> ">" <identifier> ";"
      def "VariantDeclaration" $ record [
        "types">: list $ cpp "TypeExpression",
        "name">: string],

-- <product-declaration> ::= "struct" <identifier> "{" <field-declarations> "}" ";"
      def "ProductDeclaration" $ record [
        "name">: string,
        "fields">: list $ cpp "VariableDeclaration"],

-- <container-declaration> ::= <list-declaration> | <map-declaration> | <set-declaration> | <optional-declaration>
      def "ContainerDeclaration" $ union [
        "list">: cpp "ListDeclaration",
        "map">: cpp "MapDeclaration",
        "set">: cpp "SetDeclaration",
        "optional">: cpp "OptionalDeclaration"],

-- <list-declaration> ::= "std::vector" "<" <type-expression> ">" <identifier> ";"
      def "ListDeclaration" $ record [
        "elementType">: cpp "TypeExpression",
        "name">: string],

-- <map-declaration> ::= "std::map" "<" <type-expression> "," <type-expression> ">" <identifier> ";"
      def "MapDeclaration" $ record [
        "keyType">: cpp "TypeExpression",
        "valueType">: cpp "TypeExpression",
        "name">: string],

-- <set-declaration> ::= "std::set" "<" <type-expression> ">" <identifier> ";"
      def "SetDeclaration" $ record [
        "elementType">: cpp "TypeExpression",
        "name">: string],

-- <optional-declaration> ::= "std::optional" "<" <type-expression> ">" <identifier> ";"
      def "OptionalDeclaration" $ record [
        "valueType">: cpp "TypeExpression",
        "name">: string]]

    -- Expression-related types
    expressionTypes = [
-- <expression> ::= <assignment-expression> | <expression> "," <assignment-expression>
      def "Expression" $ union [
        "assignment">: cpp "AssignmentExpression",
        "comma">: cpp "CommaExpression"],

      def "CommaExpression" $ record [
        "left">: cpp "Expression",
        "right">: cpp "AssignmentExpression"],

-- <assignment-expression> ::= <conditional-expression> | <logical-or-expression> <assignment-operator> <assignment-expression>
      def "AssignmentExpression" $ union [
        "conditional">: cpp "ConditionalExpression",
        "assignment">: cpp "ExplicitAssignment"],

      def "ExplicitAssignment" $ record [
        "left">: cpp "LogicalOrExpression",
        "op">: cpp "AssignmentOperator",
        "right">: cpp "AssignmentExpression"],

-- <assignment-operator> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "^=" | "|="
      def "AssignmentOperator" $ enum [
        "assign", "plusAssign", "minusAssign", "multiplyAssign", "divideAssign",
        "moduloAssign", "leftShiftAssign", "rightShiftAssign", "bitwiseAndAssign",
        "bitwiseXorAssign", "bitwiseOrAssign"],

-- <conditional-expression> ::= <logical-or-expression> | <logical-or-expression> "?" <expression> ":" <conditional-expression>
      def "ConditionalExpression" $ union [
        "logicalOr">: cpp "LogicalOrExpression",
        "ternary">: cpp "TernaryExpression"],

      def "TernaryExpression" $ record [
        "condition">: cpp "LogicalOrExpression",
        "trueExpr">: cpp "Expression",
        "falseExpr">: cpp "ConditionalExpression"],

-- <logical-or-expression> ::= <logical-and-expression> | <logical-or-expression> "||" <logical-and-expression>
      def "LogicalOrExpression" $ union [
        "logicalAnd">: cpp "LogicalAndExpression",
        "logicalOr">: cpp "LogicalOrOperation"],

      def "LogicalOrOperation" $ record [
        "left">: cpp "LogicalOrExpression",
        "right">: cpp "LogicalAndExpression"],

-- <logical-and-expression> ::= <inclusive-or-expression> | <logical-and-expression> "&&" <inclusive-or-expression>
      def "LogicalAndExpression" $ union [
        "inclusiveOr">: cpp "InclusiveOrExpression",
        "logicalAnd">: cpp "LogicalAndOperation"],

      def "LogicalAndOperation" $ record [
        "left">: cpp "LogicalAndExpression",
        "right">: cpp "InclusiveOrExpression"],

-- <inclusive-or-expression> ::= <exclusive-or-expression> | <inclusive-or-expression> "|" <exclusive-or-expression>
      def "InclusiveOrExpression" $ union [
        "exclusiveOr">: cpp "ExclusiveOrExpression",
        "bitwiseOr">: cpp "BitwiseOrOperation"],

      def "BitwiseOrOperation" $ record [
        "left">: cpp "InclusiveOrExpression",
        "right">: cpp "ExclusiveOrExpression"],

-- <exclusive-or-expression> ::= <and-expression> | <exclusive-or-expression> "^" <and-expression>
      def "ExclusiveOrExpression" $ union [
        "and">: cpp "AndExpression",
        "bitwiseXor">: cpp "BitwiseXorOperation"],

      def "BitwiseXorOperation" $ record [
        "left">: cpp "ExclusiveOrExpression",
        "right">: cpp "AndExpression"],

-- <and-expression> ::= <equality-expression> | <and-expression> "&" <equality-expression>
      def "AndExpression" $ union [
        "equality">: cpp "EqualityExpression",
        "bitwiseAnd">: cpp "BitwiseAndOperation"],

      def "BitwiseAndOperation" $ record [
        "left">: cpp "AndExpression",
        "right">: cpp "EqualityExpression"],

-- <equality-expression> ::= <relational-expression> | <equality-expression> "==" <relational-expression> | <equality-expression> "!=" <relational-expression>
      def "EqualityExpression" $ union [
        "relational">: cpp "RelationalExpression",
        "equal">: cpp "EqualOperation",
        "notEqual">: cpp "NotEqualOperation"],

      def "EqualOperation" $ record [
        "left">: cpp "EqualityExpression",
        "right">: cpp "RelationalExpression"],

      def "NotEqualOperation" $ record [
        "left">: cpp "EqualityExpression",
        "right">: cpp "RelationalExpression"],

-- <relational-expression> ::= <shift-expression> | <relational-expression> "<" <shift-expression> | <relational-expression> ">" <shift-expression> | <relational-expression> "<=" <shift-expression> | <relational-expression> ">=" <shift-expression>
      def "RelationalExpression" $ union [
        "shift">: cpp "ShiftExpression",
        "less">: cpp "LessOperation",
        "greater">: cpp "GreaterOperation",
        "lessEqual">: cpp "LessEqualOperation",
        "greaterEqual">: cpp "GreaterEqualOperation"],

      def "LessOperation" $ record [
        "left">: cpp "RelationalExpression",
        "right">: cpp "ShiftExpression"],

      def "GreaterOperation" $ record [
        "left">: cpp "RelationalExpression",
        "right">: cpp "ShiftExpression"],

      def "LessEqualOperation" $ record [
        "left">: cpp "RelationalExpression",
        "right">: cpp "ShiftExpression"],

      def "GreaterEqualOperation" $ record [
        "left">: cpp "RelationalExpression",
        "right">: cpp "ShiftExpression"],

-- <shift-expression> ::= <additive-expression> | <shift-expression> "<<" <additive-expression> | <shift-expression> ">>" <additive-expression>
      def "ShiftExpression" $ union [
        "additive">: cpp "AdditiveExpression",
        "leftShift">: cpp "LeftShiftOperation",
        "rightShift">: cpp "RightShiftOperation"],

      def "LeftShiftOperation" $ record [
        "left">: cpp "ShiftExpression",
        "right">: cpp "AdditiveExpression"],

      def "RightShiftOperation" $ record [
        "left">: cpp "ShiftExpression",
        "right">: cpp "AdditiveExpression"],

-- <additive-expression> ::= <multiplicative-expression> | <additive-expression> "+" <multiplicative-expression> | <additive-expression> "-" <multiplicative-expression>
      def "AdditiveExpression" $ union [
        "multiplicative">: cpp "MultiplicativeExpression",
        "add">: cpp "AddOperation",
        "subtract">: cpp "SubtractOperation"],

      def "AddOperation" $ record [
        "left">: cpp "AdditiveExpression",
        "right">: cpp "MultiplicativeExpression"],

      def "SubtractOperation" $ record [
        "left">: cpp "AdditiveExpression",
        "right">: cpp "MultiplicativeExpression"],

-- <multiplicative-expression> ::= <unary-expression> | <multiplicative-expression> "*" <unary-expression> | <multiplicative-expression> "/" <unary-expression> | <multiplicative-expression> "%" <unary-expression>
      def "MultiplicativeExpression" $ union [
        "unary">: cpp "UnaryExpression",
        "multiply">: cpp "MultiplyOperation",
        "divide">: cpp "DivideOperation",
        "modulo">: cpp "ModuloOperation"],

      def "MultiplyOperation" $ record [
        "left">: cpp "MultiplicativeExpression",
        "right">: cpp "UnaryExpression"],

      def "DivideOperation" $ record [
        "left">: cpp "MultiplicativeExpression",
        "right">: cpp "UnaryExpression"],

      def "ModuloOperation" $ record [
        "left">: cpp "MultiplicativeExpression",
        "right">: cpp "UnaryExpression"],

-- <unary-expression> ::= <postfix-expression> | <unary-operator> <unary-expression> | <sizeof-expression>
      def "UnaryExpression" $ union [
        "postfix">: cpp "PostfixExpression",
        "unaryOp">: cpp "UnaryOperation",
        "sizeof">: cpp "SizeofExpression"],

      def "UnaryOperation" $ record [
        "operator">: cpp "UnaryOperator",
        "operand">: cpp "UnaryExpression"],

-- <unary-operator> ::= "+" | "-" | "!" | "~" | "*" | "&" | "++" | "--"
      def "UnaryOperator" $ enum [
        "plus", "minus", "logicalNot", "bitwiseNot", "dereference",
        "addressOf", "preIncrement", "preDecrement"],

-- <sizeof-expression> ::= "sizeof" "(" <type-expression> ")"
      def "SizeofExpression" $ cpp "TypeExpression",

-- <postfix-expression> ::= <primary-expression> | <postfix-expression> "[" <expression> "]" | <postfix-expression> "(" <expression-list> ")" | <postfix-expression> "." <identifier> | <postfix-expression> "->" <identifier> | <postfix-expression> "++" | <postfix-expression> "--"
      def "PostfixExpression" $ union [
        "primary">: cpp "PrimaryExpression",
        "subscript">: cpp "SubscriptOperation",
        "functionCall">: cpp "FunctionCallOperation",
        "memberAccess">: cpp "MemberAccessOperation",
        "pointerMemberAccess">: cpp "PointerMemberAccessOperation",
        "postIncrement">: cpp "PostfixExpression",
        "postDecrement">: cpp "PostfixExpression"],

      def "SubscriptOperation" $ record [
        "array">: cpp "PostfixExpression",
        "index">: cpp "Expression"],

      def "FunctionCallOperation" $ record [
        "function">: cpp "PostfixExpression",
        "arguments">: list $ cpp "Expression"],

      def "MemberAccessOperation" $ record [
        "object">: cpp "PostfixExpression",
        "member">: string],

      def "PointerMemberAccessOperation" $ record [
        "pointer">: cpp "PostfixExpression",
        "member">: string],

-- <primary-expression> ::= <identifier> | <literal> | "(" <expression> ")" | <lambda-expression>
      def "PrimaryExpression" $ union [
        "identifier">: string,
        "literal">: cpp "Literal",
        "parenthesized">: cpp "Expression",
        "lambda">: cpp "LambdaExpression"],

-- <lambda-expression> ::= "[" <capture-list> "]" <lambda-parameters> <lambda-specifiers> <compound-statement>
      def "LambdaExpression" $ record [
        "captures">: cpp "CaptureList",
        "parameters">: list $ cpp "Parameter",
        "returnType">: optional $ cpp "TypeExpression",
        "body">: cpp "CompoundStatement"],

-- <capture-list> ::= <capture> | <capture> "," <capture-list> | ε | "="
      def "CaptureList" $ union [
        "captureByValue">: unit,
        "captures">: list $ cpp "Capture"],

-- <capture> ::= "&" <identifier> | <identifier>
      def "Capture" $ record [
        "name">: string,
        "byReference">: boolean],

-- <pattern-match> ::= "std::visit" "(" <visitor> "," <variant-expression> ")"
      def "PatternMatch" $ record [
        "visitor">: cpp "Visitor",
        "variant">: cpp "Expression"],

-- <visitor> ::= <lambda-expression> | <overloaded-lambdas>
      def "Visitor" $ union [
        "lambda">: cpp "LambdaExpression",
        "overloaded">: cpp "OverloadedLambdas"],

-- <overloaded-lambdas> ::= "overloaded" "{" <lambda-list> "}"
      def "OverloadedLambdas" $ list $ cpp "LambdaExpression",

-- <function-application> ::= <function-identifier> "(" <expression-list> ")"
      def "FunctionApplication" $ record [
        "function">: cpp "FunctionIdentifier",
        "arguments">: list $ cpp "Expression"],

-- <function-identifier> ::= <identifier> | <identifier> "::" <identifier>
      def "FunctionIdentifier" $ union [
        "simple">: string,
        "qualified">: cpp "QualifiedIdentifier"],

      def "QualifiedIdentifier" $ record [
        "namespace">: string,
        "name">: string]]

    -- Statement-related types
    statementTypes = [
-- <statement> ::= <labeled-statement> | <compound-statement> | <selection-statement> | <iteration-statement> | <jump-statement> | <declaration-statement> | <expression-statement>
      def "Statement" $ union [
        "labeled">: cpp "LabeledStatement",
        "compound">: cpp "CompoundStatement",
        "selection">: cpp "SelectionStatement",
        "iteration">: cpp "IterationStatement",
        "jump">: cpp "JumpStatement",
        "declaration">: cpp "VariableDeclaration",
        "expression">: cpp "Expression"],

-- <labeled-statement> ::= <identifier> ":" <statement>
      def "LabeledStatement" $ record [
        "label">: string,
        "statement">: cpp "Statement"],

-- <compound-statement> ::= "{" <statement-list> "}"
      def "CompoundStatement" $ list $ cpp "Statement",

-- <selection-statement> ::= "if" "(" <expression> ")" <statement> <else-clause>
      def "SelectionStatement" $ record [
        "condition">: cpp "Expression",
        "thenBranch">: cpp "Statement",
        "elseBranch">: optional $ cpp "Statement"],

-- <iteration-statement> ::= <while-statement> | <do-statement> | <for-statement> | <range-for-statement>
      def "IterationStatement" $ union [
        "while">: cpp "WhileStatement",
        "do">: cpp "DoStatement",
        "for">: cpp "ForStatement",
        "rangeFor">: cpp "RangeForStatement"],

-- <while-statement> ::= "while" "(" <expression> ")" <statement>
      def "WhileStatement" $ record [
        "condition">: cpp "Expression",
        "body">: cpp "Statement"],

-- <do-statement> ::= "do" <statement> "while" "(" <expression> ")" ";"
      def "DoStatement" $ record [
        "body">: cpp "Statement",
        "condition">: cpp "Expression"],

-- <for-statement> ::= "for" "(" <for-init> ";" <expression> ";" <expression> ")" <statement>
      def "ForStatement" $ record [
        "init">: cpp "ForInit",
        "condition">: cpp "Expression",
        "increment">: cpp "Expression",
        "body">: cpp "Statement"],

-- <for-init> ::= <expression> | <variable-declaration> | ε
      def "ForInit" $ union [
        "expression">: cpp "Expression",
        "declaration">: cpp "VariableDeclaration",
        "empty">: unit],

-- <range-for-statement> ::= "for" "(" <type-expression> <identifier> ":" <expression> ")" <statement>
      def "RangeForStatement" $ record [
        "type">: cpp "TypeExpression",
        "variable">: string,
        "range">: cpp "Expression",
        "body">: cpp "Statement"],

-- <jump-statement> ::= "break" ";" | "continue" ";" | "return" <expression> ";" | "return" ";"
      def "JumpStatement" $ union [
        "break">: unit,
        "continue">: unit,
        "returnValue">: cpp "Expression",
        "returnVoid">: unit],

-- <expression-statement> ::= <expression> ";"
      def "ExpressionStatement" $ cpp "Expression"]

    -- Type-related types
    typeTypes = [
-- <type-expression> ::= <basic-type> | <qualified-type> | <template-type> | <function-type> | <auto-type>
      def "TypeExpression" $ union [
        "basic">: cpp "BasicType",
        "qualified">: cpp "QualifiedType",
        "template">: cpp "TemplateType",
        "function">: cpp "FunctionType",
        "auto">: unit],

-- <basic-type> ::= "void" | "bool" | "char" | "int" | "float" | "double" | "std::string" | <identifier>
      def "BasicType" $ union [
        "void">: unit,
        "bool">: unit,
        "char">: unit,
        "int">: unit,
        "float">: unit,
        "double">: unit,
        "string">: unit,
        "auto">: unit,
        "named">: string],

-- <qualified-type> ::= <type-expression> <type-qualifier>
      def "QualifiedType" $ record [
        "baseType">: cpp "TypeExpression",
        "qualifier">: cpp "TypeQualifier"],

-- <type-qualifier> ::= "const" | "&" | "&&" | "*"
      def "TypeQualifier" $ enum ["const", "lvalueRef", "rvalueRef", "pointer"],

-- <template-type> ::= <identifier> "<" <template-arg-list> ">"
      def "TemplateType" $ record [
        "name">: string,
        "arguments">: list $ cpp "TemplateArgument"],

-- <template-arg> ::= <type-expression> | <expression>
      def "TemplateArgument" $ union [
        "type">: cpp "TypeExpression",
        "value">: cpp "Expression"],

-- <function-type> ::= <type-expression> "(" <parameter-list> ")"
      def "FunctionType" $ record [
        "returnType">: cpp "TypeExpression",
        "parameters">: list $ cpp "Parameter"]]

    -- Literal-related types
    literalTypes = [
-- <literal> ::= <integer-literal> | <floating-literal> | <character-literal> | <string-literal> | <boolean-literal> | <null-literal>
      def "Literal" $ union [
        "integer">: cpp "IntegerLiteral",
        "floating">: cpp "FloatingLiteral",
        "character">: cpp "CharacterLiteral",
        "string">: cpp "StringLiteral",
        "boolean">: cpp "BooleanLiteral",
        "null">: unit],

-- <integer-literal> ::= <decimal-literal> | <hexadecimal-literal> | <octal-literal> | <binary-literal>
      def "IntegerLiteral" $ union [
        "decimal">: bigint,
        "hexadecimal">: string,
        "octal">: string,
        "binary">: string],

-- <floating-literal> ::= <fractional-constant> <exponent-part> | <digit-sequence> <exponent-part>
      def "FloatingLiteral" $ bigfloat,

-- <character-literal> ::= "'" <c-char> "'"
      def "CharacterLiteral" $ string,

-- <string-literal> ::= "\"" <s-char-sequence> "\""
      def "StringLiteral" $ string,

-- <boolean-literal> ::= "true" | "false"
      def "BooleanLiteral" $ boolean]

    -- Container-related types
    containers = [
      def "Vector" $ record [
        "elementType">: cpp "TypeExpression",
        "elements">: list $ cpp "Expression"],

      def "Map" $ record [
        "keyType">: cpp "TypeExpression",
        "valueType">: cpp "TypeExpression",
        "entries">: list $ cpp "MapEntry"],

      def "MapEntry" $ record [
        "key">: cpp "Expression",
        "value">: cpp "Expression"],

      def "Set" $ record [
        "elementType">: cpp "TypeExpression",
        "elements">: list $ cpp "Expression"],

      def "Optional" $ record [
        "valueType">: cpp "TypeExpression",
        "value">: optional $ cpp "Expression"]]

    -- Operator types
    operatorTypes = [
      def "BinaryOperator" $ enum [
        "plus", "minus", "multiply", "divide", "modulo",
        "bitwiseAnd", "bitwiseOr", "bitwiseXor",
        "logicalAnd", "logicalOr",
        "equal", "notEqual", "less", "greater", "lessEqual", "greaterEqual",
        "leftShift", "rightShift"]]

    -- Utility types
    utilities = [
      def "Identifier" string,

      def "Comment" $ record [
        "text">: string,
        "isMultiline">: boolean]]