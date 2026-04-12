-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Sources.Cpp.Serde where

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
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
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
import Hydra.Ast
import qualified Hydra.Ext.Cpp.Syntax as Cpp
import qualified Hydra.Ext.Sources.Cpp.Syntax as CppSyntax


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.cpp.serde"

module_ :: Module
module_ = Module ns definitions
    [Serialization.ns]
    (CppSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting C++ AST to abstract expressions"
  where
    definitions = [
      toDefinition encodeAccessSpecifier,
      toDefinition encodeAddOperation,
      toDefinition encodeAdditiveExpression,
      toDefinition encodeAndExpression,
      toDefinition encodeAssignmentExpression,
      toDefinition encodeAssignmentOperator,
      toDefinition encodeBaseSpecifier,
      toDefinition encodeBasicType,
      toDefinition encodeBitwiseAndOperation,
      toDefinition encodeBitwiseOrOperation,
      toDefinition encodeBitwiseXorOperation,
      toDefinition encodeBooleanLiteral,
      toDefinition encodeCapture,
      toDefinition encodeCaptureList,
      toDefinition encodeCaseStatement,
      toDefinition encodeCaseValue,
      toDefinition encodeClassBody,
      toDefinition encodeClassDeclaration,
      toDefinition encodeClassKey,
      toDefinition encodeClassSpecifier,
      toDefinition encodeCommaExpression,
      toDefinition encodeComment,
      toDefinition encodeCompoundStatement,
      toDefinition encodeConditionalExpression,
      toDefinition encodeConstructorDeclaration,
      toDefinition encodeDeclaration,
      toDefinition encodeDefineDirective,
      toDefinition encodeDestructorDeclaration,
      toDefinition encodeDivideOperation,
      toDefinition encodeDoStatement,
      toDefinition encodeElifDirective,
      toDefinition encodeElseDirective,
      toDefinition encodeEndifDirective,
      toDefinition encodeEqualOperation,
      toDefinition encodeEqualityExpression,
      toDefinition encodeErrorDirective,
      toDefinition encodeExclusiveOrExpression,
      toDefinition encodeExplicitAssignment,
      toDefinition encodeExpression,
      toDefinition encodeForInit,
      toDefinition encodeForStatement,
      toDefinition encodeFunctionApplication,
      toDefinition encodeFunctionBody,
      toDefinition encodeFunctionCallOperation,
      toDefinition encodeFunctionDeclaration,
      toDefinition encodeFunctionIdentifier,
      toDefinition encodeFunctionSpecifierPrefix,
      toDefinition encodeFunctionSpecifierSuffix,
      toDefinition encodeFunctionType,
      toDefinition encodeGreaterEqualOperation,
      toDefinition encodeGreaterOperation,
      toDefinition encodeIfDirective,
      toDefinition encodeIfdefDirective,
      toDefinition encodeIfndefDirective,
      toDefinition encodeIncludeDirective,
      toDefinition encodeInclusiveOrExpression,
      toDefinition encodeIntegerLiteral,
      toDefinition encodeIterationStatement,
      toDefinition encodeJumpStatement,
      toDefinition encodeLabeledStatement,
      toDefinition encodeLambdaExpression,
      toDefinition encodeLeftShiftOperation,
      toDefinition encodeLessEqualOperation,
      toDefinition encodeLessOperation,
      toDefinition encodeLineDirective,
      toDefinition encodeLiteral,
      toDefinition encodeLogicalAndExpression,
      toDefinition encodeLogicalAndOperation,
      toDefinition encodeLogicalOrExpression,
      toDefinition encodeLogicalOrOperation,
      toDefinition encodeMap,
      toDefinition encodeMapEntry,
      toDefinition encodeMemberAccessOperation,
      toDefinition encodeMemberDeclaration,
      toDefinition encodeMemberSpecification,
      toDefinition encodeMemInitializer,
      toDefinition encodeModuloOperation,
      toDefinition encodeMultiplicativeExpression,
      toDefinition encodeMultiplyOperation,
      toDefinition encodeNamespaceDeclaration,
      toDefinition encodeNotEqualOperation,
      toDefinition encodeOptional,
      toDefinition encodeOverloadedLambdas,
      toDefinition encodeParameter,
      toDefinition encodePatternMatch,
      toDefinition encodePointerMemberAccessOperation,
      toDefinition encodePostfixExpression,
      toDefinition encodePragmaDirective,
      toDefinition encodePreprocessorDirective,
      toDefinition encodePrimaryExpression,
      toDefinition encodeProgram,
      toDefinition encodeQualifiedIdentifier,
      toDefinition encodeQualifiedType,
      toDefinition encodeRangeForStatement,
      toDefinition encodeRelationalExpression,
      toDefinition encodeRightShiftOperation,
      toDefinition encodeSelectionStatement,
      toDefinition encodeSet,
      toDefinition encodeShiftExpression,
      toDefinition encodeSizeofExpression,
      toDefinition encodeStatement,
      toDefinition encodeSubscriptOperation,
      toDefinition encodeSubtractOperation,
      toDefinition encodeSwitchStatement,
      toDefinition encodeTemplateArgument,
      toDefinition encodeTemplateDeclaration,
      toDefinition encodeTemplateFunctionCallOperation,
      toDefinition encodeTemplateType,
      toDefinition encodeTernaryExpression,
      toDefinition encodeToCppComments,
      toDefinition encodeTypeExpression,
      toDefinition encodeTypedefDeclaration,
      toDefinition encodeUnaryExpression,
      toDefinition encodeUnaryOperation,
      toDefinition encodeUnaryOperator,
      toDefinition encodeUndefDirective,
      toDefinition encodeVariableDeclaration,
      toDefinition encodeVector,
      toDefinition encodeVisitor,
      toDefinition encodeWarningDirective,
      toDefinition encodeWhileStatement]


encodeAccessSpecifier :: TTermDefinition (Cpp.AccessSpecifier -> Expr)
encodeAccessSpecifier = define "encodeAccessSpecifier" $
  doc "Convert an access specifier to an expression" $
  lambda "a" $
    cases Cpp._AccessSpecifier (var "a") Nothing [
      Cpp._AccessSpecifier_public>>: constant $ Serialization.cst @@ string "public",
      Cpp._AccessSpecifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Cpp._AccessSpecifier_private>>: constant $ Serialization.cst @@ string "private",
      Cpp._AccessSpecifier_none>>: constant $ Serialization.cst @@ string ""]

encodeAddOperation :: TTermDefinition (Cpp.AddOperation -> Expr)
encodeAddOperation = define "encodeAddOperation" $
  doc "Convert an add operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._AddOperation Cpp._AddOperation_left @@ var "op",
    "right">: project Cpp._AddOperation Cpp._AddOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeAdditiveExpression @@ var "left",
      Serialization.cst @@ string "+",
      encodeMultiplicativeExpression @@ var "right"]

encodeAdditiveExpression :: TTermDefinition (Cpp.AdditiveExpression -> Expr)
encodeAdditiveExpression = define "encodeAdditiveExpression" $
  doc "Convert an additive expression to an expression" $
  lambda "e" $
    cases Cpp._AdditiveExpression (var "e") Nothing [
      Cpp._AdditiveExpression_multiplicative>>: lambda "m" $ encodeMultiplicativeExpression @@ var "m",
      Cpp._AdditiveExpression_add>>: lambda "a" $ encodeAddOperation @@ var "a",
      Cpp._AdditiveExpression_subtract>>: lambda "s" $ encodeSubtractOperation @@ var "s"]

encodeAndExpression :: TTermDefinition (Cpp.AndExpression -> Expr)
encodeAndExpression = define "encodeAndExpression" $
  doc "Convert an and expression to an expression" $
  lambda "e" $
    cases Cpp._AndExpression (var "e") Nothing [
      Cpp._AndExpression_equality>>: lambda "eq" $ encodeEqualityExpression @@ var "eq",
      Cpp._AndExpression_bitwiseAnd>>: lambda "a" $ encodeBitwiseAndOperation @@ var "a"]

encodeAssignmentExpression :: TTermDefinition (Cpp.AssignmentExpression -> Expr)
encodeAssignmentExpression = define "encodeAssignmentExpression" $
  doc "Convert an assignment expression to an expression" $
  lambda "a" $
    cases Cpp._AssignmentExpression (var "a") Nothing [
      Cpp._AssignmentExpression_conditional>>: lambda "c" $ encodeConditionalExpression @@ var "c",
      Cpp._AssignmentExpression_assignment>>: lambda "e" $ encodeExplicitAssignment @@ var "e"]

encodeAssignmentOperator :: TTermDefinition (Cpp.AssignmentOperator -> Expr)
encodeAssignmentOperator = define "encodeAssignmentOperator" $
  doc "Convert an assignment operator to an expression" $
  lambda "op" $
    cases Cpp._AssignmentOperator (var "op") Nothing [
      Cpp._AssignmentOperator_assign>>: constant $ Serialization.cst @@ string "=",
      Cpp._AssignmentOperator_plusAssign>>: constant $ Serialization.cst @@ string "+=",
      Cpp._AssignmentOperator_minusAssign>>: constant $ Serialization.cst @@ string "-=",
      Cpp._AssignmentOperator_multiplyAssign>>: constant $ Serialization.cst @@ string "*=",
      Cpp._AssignmentOperator_divideAssign>>: constant $ Serialization.cst @@ string "/=",
      Cpp._AssignmentOperator_moduloAssign>>: constant $ Serialization.cst @@ string "%=",
      Cpp._AssignmentOperator_leftShiftAssign>>: constant $ Serialization.cst @@ string "<<=",
      Cpp._AssignmentOperator_rightShiftAssign>>: constant $ Serialization.cst @@ string ">>=",
      Cpp._AssignmentOperator_bitwiseAndAssign>>: constant $ Serialization.cst @@ string "&=",
      Cpp._AssignmentOperator_bitwiseXorAssign>>: constant $ Serialization.cst @@ string "^=",
      Cpp._AssignmentOperator_bitwiseOrAssign>>: constant $ Serialization.cst @@ string "|="]

encodeBaseSpecifier :: TTermDefinition (Cpp.BaseSpecifier -> Expr)
encodeBaseSpecifier = define "encodeBaseSpecifier" $
  doc "Convert a base specifier to an expression" $
  lambda "bs" $ lets [
    "access">: project Cpp._BaseSpecifier Cpp._BaseSpecifier_access @@ var "bs",
    "name">: project Cpp._BaseSpecifier Cpp._BaseSpecifier_name @@ var "bs"] $
    Serialization.spaceSep @@ list [encodeAccessSpecifier @@ var "access", Serialization.cst @@ var "name"]

encodeBasicType :: TTermDefinition (Cpp.BasicType -> Expr)
encodeBasicType = define "encodeBasicType" $
  doc "Convert a basic type to an expression" $
  lambda "t" $
    cases Cpp._BasicType (var "t") Nothing [
      Cpp._BasicType_void>>: constant $ Serialization.cst @@ string "void",
      Cpp._BasicType_bool>>: constant $ Serialization.cst @@ string "bool",
      Cpp._BasicType_char>>: constant $ Serialization.cst @@ string "char",
      Cpp._BasicType_int>>: constant $ Serialization.cst @@ string "int",
      Cpp._BasicType_float>>: constant $ Serialization.cst @@ string "float",
      Cpp._BasicType_double>>: constant $ Serialization.cst @@ string "double",
      Cpp._BasicType_string>>: constant $ Serialization.cst @@ string "std::string",
      Cpp._BasicType_auto>>: constant $ Serialization.cst @@ string "auto",
      Cpp._BasicType_named>>: lambda "name" $ Serialization.cst @@ var "name"]

encodeBitwiseAndOperation :: TTermDefinition (Cpp.BitwiseAndOperation -> Expr)
encodeBitwiseAndOperation = define "encodeBitwiseAndOperation" $
  doc "Convert a bitwise and operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._BitwiseAndOperation Cpp._BitwiseAndOperation_left @@ var "op",
    "right">: project Cpp._BitwiseAndOperation Cpp._BitwiseAndOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeAndExpression @@ var "left",
      Serialization.cst @@ string "&",
      encodeEqualityExpression @@ var "right"]

encodeBitwiseOrOperation :: TTermDefinition (Cpp.BitwiseOrOperation -> Expr)
encodeBitwiseOrOperation = define "encodeBitwiseOrOperation" $
  doc "Convert a bitwise or operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._BitwiseOrOperation Cpp._BitwiseOrOperation_left @@ var "op",
    "right">: project Cpp._BitwiseOrOperation Cpp._BitwiseOrOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeInclusiveOrExpression @@ var "left",
      Serialization.cst @@ string "|",
      encodeExclusiveOrExpression @@ var "right"]

encodeBitwiseXorOperation :: TTermDefinition (Cpp.BitwiseXorOperation -> Expr)
encodeBitwiseXorOperation = define "encodeBitwiseXorOperation" $
  doc "Convert a bitwise xor operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._BitwiseXorOperation Cpp._BitwiseXorOperation_left @@ var "op",
    "right">: project Cpp._BitwiseXorOperation Cpp._BitwiseXorOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeExclusiveOrExpression @@ var "left",
      Serialization.cst @@ string "^",
      encodeAndExpression @@ var "right"]

encodeBooleanLiteral :: TTermDefinition (Cpp.BooleanLiteral -> Expr)
encodeBooleanLiteral = define "encodeBooleanLiteral" $
  doc "Convert a boolean literal to an expression" $
  lambda "bl" $
    Logic.ifElse (unwrap Cpp._BooleanLiteral @@ var "bl")
      (Serialization.cst @@ string "true")
      (Serialization.cst @@ string "false")

encodeCapture :: TTermDefinition (Cpp.Capture -> Expr)
encodeCapture = define "encodeCapture" $
  doc "Convert a capture to an expression" $
  lambda "cap" $ lets [
    "name">: project Cpp._Capture Cpp._Capture_name @@ var "cap",
    "byRef">: project Cpp._Capture Cpp._Capture_byReference @@ var "cap"] $
    Logic.ifElse (var "byRef")
      (Serialization.cst @@ (Strings.cat2 (string "&") (var "name")))
      (Serialization.cst @@ var "name")

encodeCaptureList :: TTermDefinition (Cpp.CaptureList -> Expr)
encodeCaptureList = define "encodeCaptureList" $
  doc "Convert a capture list to an expression" $
  lambda "cl" $
    cases Cpp._CaptureList (var "cl") Nothing [
      Cpp._CaptureList_captureByValue>>: constant $ Serialization.cst @@ string "[=]",
      Cpp._CaptureList_captures>>: lambda "cs" $
        Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map encodeCapture (var "cs"))]

encodeCaseStatement :: TTermDefinition (Cpp.CaseStatement -> Expr)
encodeCaseStatement = define "encodeCaseStatement" $
  doc "Convert a case statement to an expression" $
  lambda "stmt" $
    cases Cpp._CaseStatement (var "stmt") Nothing [
      Cpp._CaseStatement_case>>: lambda "cv" $ encodeCaseValue @@ var "cv",
      Cpp._CaseStatement_default>>: lambda "s" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "default:",
          encodeStatement @@ var "s"]]

encodeCaseValue :: TTermDefinition (Cpp.CaseValue -> Expr)
encodeCaseValue = define "encodeCaseValue" $
  doc "Convert a case value to an expression" $
  lambda "cv" $ lets [
    "value">: project Cpp._CaseValue Cpp._CaseValue_value @@ var "cv",
    "statement">: project Cpp._CaseValue Cpp._CaseValue_statement @@ var "cv"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "case",
      Serialization.noSep @@ list [encodeExpression @@ var "value", Serialization.cst @@ string ":"],
      encodeStatement @@ var "statement"]

encodeClassBody :: TTermDefinition (Bool -> Cpp.ClassBody -> Expr)
encodeClassBody = define "encodeClassBody" $
  doc "Convert a class body to an expression" $
  lambda "commas" $ lambda "cb" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.doubleNewlineSep @@ (Lists.map (encodeMemberSpecification @@ var "commas") (unwrap Cpp._ClassBody @@ var "cb")))

encodeClassDeclaration :: TTermDefinition (Cpp.ClassDeclaration -> Expr)
encodeClassDeclaration = define "encodeClassDeclaration" $
  doc "Convert a class declaration to an expression" $
  lambda "cd" $ lets [
    "spec">: project Cpp._ClassDeclaration Cpp._ClassDeclaration_specifier @@ var "cd",
    "mbody">: project Cpp._ClassDeclaration Cpp._ClassDeclaration_body @@ var "cd",
    "key">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_key @@ var "spec",
    "isEnum">: Logic.or
      (Equality.equal (var "key") (inject Cpp._ClassKey Cpp._ClassKey_enum unit))
      (Equality.equal (var "key") (inject Cpp._ClassKey Cpp._ClassKey_enumClass unit))] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ (Maybes.cat $ list [
      just (encodeClassSpecifier @@ var "spec"),
      Maybes.map (lambda "body" $ encodeClassBody @@ var "isEnum" @@ var "body") (var "mbody")]))

encodeClassKey :: TTermDefinition (Cpp.ClassKey -> Expr)
encodeClassKey = define "encodeClassKey" $
  doc "Convert a class key to an expression" $
  lambda "k" $
    cases Cpp._ClassKey (var "k") Nothing [
      Cpp._ClassKey_class>>: constant $ Serialization.cst @@ string "class",
      Cpp._ClassKey_enum>>: constant $ Serialization.cst @@ string "enum",
      Cpp._ClassKey_enumClass>>: constant $ Serialization.cst @@ string "enum class",
      Cpp._ClassKey_struct>>: constant $ Serialization.cst @@ string "struct"]

encodeClassSpecifier :: TTermDefinition (Cpp.ClassSpecifier -> Expr)
encodeClassSpecifier = define "encodeClassSpecifier" $
  doc "Convert a class specifier to an expression" $
  lambda "cs" $ lets [
    "key">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_key @@ var "cs",
    "name">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_name @@ var "cs",
    "inheritance">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_inheritance @@ var "cs"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [encodeClassKey @@ var "key", Serialization.cst @@ var "name"],
      Logic.ifElse (Lists.null (var "inheritance"))
        (list ([] :: [TTerm Expr]))
        (list [Serialization.cst @@ string ":",
          Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeBaseSpecifier (var "inheritance"))])])

encodeCommaExpression :: TTermDefinition (Cpp.CommaExpression -> Expr)
encodeCommaExpression = define "encodeCommaExpression" $
  doc "Convert a comma expression to an expression" $
  lambda "ce" $ lets [
    "left">: project Cpp._CommaExpression Cpp._CommaExpression_left @@ var "ce",
    "right">: project Cpp._CommaExpression Cpp._CommaExpression_right @@ var "ce"] $
    Serialization.spaceSep @@ list [
      encodeExpression @@ var "left",
      Serialization.cst @@ string ",",
      encodeAssignmentExpression @@ var "right"]

encodeComment :: TTermDefinition (Cpp.Comment -> Expr)
encodeComment = define "encodeComment" $
  doc "Convert a comment to an expression" $
  lambda "c" $ lets [
    "text">: project Cpp._Comment Cpp._Comment_text @@ var "c",
    "isMultiline">: project Cpp._Comment Cpp._Comment_isMultiline @@ var "c"] $
    Serialization.cst @@ (encodeToCppComments @@ var "text" @@ var "isMultiline")

encodeCompoundStatement :: TTermDefinition (Cpp.CompoundStatement -> Expr)
encodeCompoundStatement = define "encodeCompoundStatement" $
  doc "Convert a compound statement to an expression" $
  lambda "cs" $
    Serialization.curlyBracesList @@ (just (string "")) @@ Serialization.fullBlockStyle @@
      (Lists.map encodeStatement (unwrap Cpp._CompoundStatement @@ var "cs"))

encodeConditionalExpression :: TTermDefinition (Cpp.ConditionalExpression -> Expr)
encodeConditionalExpression = define "encodeConditionalExpression" $
  doc "Convert a conditional expression to an expression" $
  lambda "c" $
    cases Cpp._ConditionalExpression (var "c") Nothing [
      Cpp._ConditionalExpression_logicalOr>>: lambda "l" $ encodeLogicalOrExpression @@ var "l",
      Cpp._ConditionalExpression_ternary>>: lambda "t" $ encodeTernaryExpression @@ var "t"]

encodeConstructorDeclaration :: TTermDefinition (Cpp.ConstructorDeclaration -> Expr)
encodeConstructorDeclaration = define "encodeConstructorDeclaration" $
  doc "Convert a constructor declaration to an expression" $
  lambda "cd" $ lets [
    "name">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_name @@ var "cd",
    "params">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_parameters @@ var "cd",
    "inits">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_initializers @@ var "cd",
    "body">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_body @@ var "cd"] $
    Serialization.spaceSep @@ (Maybes.cat $ list [
      just (Serialization.noSep @@ list [
        Serialization.cst @@ var "name",
        Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeParameter (var "params")))]),
      Logic.ifElse (Lists.null (var "inits"))
        nothing
        (just (Serialization.spaceSep @@ list [
          Serialization.cst @@ string ":",
          Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeMemInitializer (var "inits"))])),
      just (encodeFunctionBody @@ var "body")])

encodeDeclaration :: TTermDefinition (Cpp.Declaration -> Expr)
encodeDeclaration = define "encodeDeclaration" $
  doc "Convert a declaration to an expression" $
  lambda "d" $
    cases Cpp._Declaration (var "d") Nothing [
      Cpp._Declaration_preprocessor>>: lambda "p" $ encodePreprocessorDirective @@ var "p",
      Cpp._Declaration_class>>: lambda "c" $ encodeClassDeclaration @@ var "c",
      Cpp._Declaration_function>>: lambda "f" $ encodeFunctionDeclaration @@ var "f",
      Cpp._Declaration_variable>>: lambda "v" $ encodeVariableDeclaration @@ false @@ var "v",
      Cpp._Declaration_typedef>>: lambda "t" $ encodeTypedefDeclaration @@ var "t",
      Cpp._Declaration_namespace>>: lambda "n" $ encodeNamespaceDeclaration @@ var "n",
      Cpp._Declaration_template>>: lambda "t" $ encodeTemplateDeclaration @@ var "t"]

encodeDefineDirective :: TTermDefinition (Cpp.DefineDirective -> Expr)
encodeDefineDirective = define "encodeDefineDirective" $
  doc "Convert a define directive to an expression" $
  lambda "dd" $ lets [
    "name">: project Cpp._DefineDirective Cpp._DefineDirective_name @@ var "dd",
    "params">: project Cpp._DefineDirective Cpp._DefineDirective_parameters @@ var "dd",
    "replacement">: project Cpp._DefineDirective Cpp._DefineDirective_replacement @@ var "dd"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [Serialization.cst @@ string "#define", Serialization.cst @@ var "name"],
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "ps" $ list [Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map (lambda "p" $ Serialization.cst @@ var "p") (var "ps")))])
        (var "params"),
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "r" $ list [Serialization.cst @@ var "r"])
        (var "replacement")])

encodeDestructorDeclaration :: TTermDefinition (Cpp.DestructorDeclaration -> Expr)
encodeDestructorDeclaration = define "encodeDestructorDeclaration" $
  doc "Convert a destructor declaration to an expression" $
  lambda "dd" $ lets [
    "prefixSpecs">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_prefixSpecifiers @@ var "dd",
    "name">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_name @@ var "dd",
    "suffixSpecs">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_suffixSpecifiers @@ var "dd",
    "body">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_body @@ var "dd"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      Lists.map encodeFunctionSpecifierPrefix (var "prefixSpecs"),
      list [Serialization.noSep @@ list [
        Serialization.cst @@ (Strings.cat2 (string "~") (var "name")),
        Serialization.parens @@ (Serialization.cst @@ string "")]],
      Lists.map encodeFunctionSpecifierSuffix (var "suffixSpecs"),
      list [encodeFunctionBody @@ var "body"]])

encodeDivideOperation :: TTermDefinition (Cpp.DivideOperation -> Expr)
encodeDivideOperation = define "encodeDivideOperation" $
  doc "Convert a divide operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._DivideOperation Cpp._DivideOperation_left @@ var "op",
    "right">: project Cpp._DivideOperation Cpp._DivideOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeMultiplicativeExpression @@ var "left",
      Serialization.cst @@ string "/",
      encodeUnaryExpression @@ var "right"]

encodeDoStatement :: TTermDefinition (Cpp.DoStatement -> Expr)
encodeDoStatement = define "encodeDoStatement" $
  doc "Convert a do statement to an expression" $
  lambda "ds" $ lets [
    "body">: project Cpp._DoStatement Cpp._DoStatement_body @@ var "ds",
    "cond">: project Cpp._DoStatement Cpp._DoStatement_condition @@ var "ds"] $
    Serialization.newlineSep @@ list [
      Serialization.cst @@ string "do",
      encodeStatement @@ var "body",
      Serialization.withSemi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "while",
        Serialization.parens @@ (encodeExpression @@ var "cond")])]

encodeElifDirective :: TTermDefinition (Cpp.ElifDirective -> Expr)
encodeElifDirective = define "encodeElifDirective" $
  doc "Convert an elif directive to an expression" $
  lambda "ed" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#elif",
      Serialization.cst @@ (project Cpp._ElifDirective Cpp._ElifDirective_condition @@ var "ed")]

encodeElseDirective :: TTermDefinition (Cpp.ElseDirective -> Expr)
encodeElseDirective = define "encodeElseDirective" $
  doc "Convert an else directive to an expression" $
  lambda "ed" $ Serialization.cst @@ string "#else"

encodeEndifDirective :: TTermDefinition (Cpp.EndifDirective -> Expr)
encodeEndifDirective = define "encodeEndifDirective" $
  doc "Convert an endif directive to an expression" $
  lambda "ed" $ Serialization.cst @@ string "#endif"

encodeEqualOperation :: TTermDefinition (Cpp.EqualOperation -> Expr)
encodeEqualOperation = define "encodeEqualOperation" $
  doc "Convert an equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._EqualOperation Cpp._EqualOperation_left @@ var "op",
    "right">: project Cpp._EqualOperation Cpp._EqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeEqualityExpression @@ var "left",
      Serialization.cst @@ string "==",
      encodeRelationalExpression @@ var "right"]

encodeEqualityExpression :: TTermDefinition (Cpp.EqualityExpression -> Expr)
encodeEqualityExpression = define "encodeEqualityExpression" $
  doc "Convert an equality expression to an expression" $
  lambda "e" $
    cases Cpp._EqualityExpression (var "e") Nothing [
      Cpp._EqualityExpression_relational>>: lambda "r" $ encodeRelationalExpression @@ var "r",
      Cpp._EqualityExpression_equal>>: lambda "eq" $ encodeEqualOperation @@ var "eq",
      Cpp._EqualityExpression_notEqual>>: lambda "ne" $ encodeNotEqualOperation @@ var "ne"]

encodeErrorDirective :: TTermDefinition (Cpp.ErrorDirective -> Expr)
encodeErrorDirective = define "encodeErrorDirective" $
  doc "Convert an error directive to an expression" $
  lambda "ed" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#error",
      Serialization.cst @@ (project Cpp._ErrorDirective Cpp._ErrorDirective_message @@ var "ed")]

encodeExclusiveOrExpression :: TTermDefinition (Cpp.ExclusiveOrExpression -> Expr)
encodeExclusiveOrExpression = define "encodeExclusiveOrExpression" $
  doc "Convert an exclusive or expression to an expression" $
  lambda "e" $
    cases Cpp._ExclusiveOrExpression (var "e") Nothing [
      Cpp._ExclusiveOrExpression_and>>: lambda "a" $ encodeAndExpression @@ var "a",
      Cpp._ExclusiveOrExpression_bitwiseXor>>: lambda "x" $ encodeBitwiseXorOperation @@ var "x"]

encodeExplicitAssignment :: TTermDefinition (Cpp.ExplicitAssignment -> Expr)
encodeExplicitAssignment = define "encodeExplicitAssignment" $
  doc "Convert an explicit assignment to an expression" $
  lambda "ea" $ lets [
    "left">: project Cpp._ExplicitAssignment Cpp._ExplicitAssignment_left @@ var "ea",
    "op">: project Cpp._ExplicitAssignment Cpp._ExplicitAssignment_op @@ var "ea",
    "right">: project Cpp._ExplicitAssignment Cpp._ExplicitAssignment_right @@ var "ea"] $
    Serialization.spaceSep @@ list [
      encodeLogicalOrExpression @@ var "left",
      encodeAssignmentOperator @@ var "op",
      encodeAssignmentExpression @@ var "right"]

encodeExpression :: TTermDefinition (Cpp.Expression -> Expr)
encodeExpression = define "encodeExpression" $
  doc "Convert an expression to an expression" $
  lambda "e" $
    cases Cpp._Expression (var "e") Nothing [
      Cpp._Expression_assignment>>: lambda "a" $ encodeAssignmentExpression @@ var "a",
      Cpp._Expression_comma>>: lambda "c" $ encodeCommaExpression @@ var "c"]

encodeForInit :: TTermDefinition (Cpp.ForInit -> Expr)
encodeForInit = define "encodeForInit" $
  doc "Convert a for-init to an expression" $
  lambda "i" $
    cases Cpp._ForInit (var "i") Nothing [
      Cpp._ForInit_expression>>: lambda "e" $ encodeExpression @@ var "e",
      Cpp._ForInit_declaration>>: lambda "d" $ encodeVariableDeclaration @@ false @@ var "d",
      Cpp._ForInit_empty>>: constant $ Serialization.cst @@ string ""]

encodeForStatement :: TTermDefinition (Cpp.ForStatement -> Expr)
encodeForStatement = define "encodeForStatement" $
  doc "Convert a for statement to an expression" $
  lambda "fs" $ lets [
    "init">: project Cpp._ForStatement Cpp._ForStatement_init @@ var "fs",
    "cond">: project Cpp._ForStatement Cpp._ForStatement_condition @@ var "fs",
    "inc">: project Cpp._ForStatement Cpp._ForStatement_increment @@ var "fs",
    "body">: project Cpp._ForStatement Cpp._ForStatement_body @@ var "fs"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "for",
        Serialization.parens @@ (Serialization.noSep @@ list [
          encodeForInit @@ var "init",
          Serialization.cst @@ string ";",
          encodeExpression @@ var "cond",
          Serialization.cst @@ string ";",
          encodeExpression @@ var "inc"])],
      encodeStatement @@ var "body"]

encodeFunctionApplication :: TTermDefinition (Cpp.FunctionApplication -> Expr)
encodeFunctionApplication = define "encodeFunctionApplication" $
  doc "Convert a function application to an expression" $
  lambda "fa" $ lets [
    "func">: project Cpp._FunctionApplication Cpp._FunctionApplication_function @@ var "fa",
    "args">: project Cpp._FunctionApplication Cpp._FunctionApplication_arguments @@ var "fa"] $
    Serialization.spaceSep @@ list [
      encodeFunctionIdentifier @@ var "func",
      Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeExpression (var "args")))]

encodeFunctionBody :: TTermDefinition (Cpp.FunctionBody -> Expr)
encodeFunctionBody = define "encodeFunctionBody" $
  doc "Convert a function body to an expression" $
  lambda "b" $
    cases Cpp._FunctionBody (var "b") Nothing [
      Cpp._FunctionBody_compound>>: lambda "c" $ encodeCompoundStatement @@ var "c",
      Cpp._FunctionBody_declaration>>: constant $ Serialization.cst @@ string ";",
      Cpp._FunctionBody_pure>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "= 0"),
      Cpp._FunctionBody_default>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "= default")]

encodeFunctionCallOperation :: TTermDefinition (Cpp.FunctionCallOperation -> Expr)
encodeFunctionCallOperation = define "encodeFunctionCallOperation" $
  doc "Convert a function call operation to an expression" $
  lambda "fco" $ lets [
    "func">: project Cpp._FunctionCallOperation Cpp._FunctionCallOperation_function @@ var "fco",
    "args">: project Cpp._FunctionCallOperation Cpp._FunctionCallOperation_arguments @@ var "fco"] $
    Serialization.noSep @@ list [
      encodePostfixExpression @@ var "func",
      Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeExpression (var "args")))]

encodeFunctionDeclaration :: TTermDefinition (Cpp.FunctionDeclaration -> Expr)
encodeFunctionDeclaration = define "encodeFunctionDeclaration" $
  doc "Convert a function declaration to an expression" $
  lambda "fd" $ lets [
    "prefixSpecs">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_prefixSpecifiers @@ var "fd",
    "retType">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_returnType @@ var "fd",
    "name">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_name @@ var "fd",
    "params">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_parameters @@ var "fd",
    "suffixSpecs">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_suffixSpecifiers @@ var "fd",
    "body">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_body @@ var "fd"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      Lists.map encodeFunctionSpecifierPrefix (var "prefixSpecs"),
      list [
        encodeTypeExpression @@ var "retType",
        Serialization.noSep @@ list [
          Serialization.cst @@ var "name",
          Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeParameter (var "params")))]],
      Lists.map encodeFunctionSpecifierSuffix (var "suffixSpecs"),
      list [encodeFunctionBody @@ var "body"]])

encodeFunctionIdentifier :: TTermDefinition (Cpp.FunctionIdentifier -> Expr)
encodeFunctionIdentifier = define "encodeFunctionIdentifier" $
  doc "Convert a function identifier to an expression" $
  lambda "f" $
    cases Cpp._FunctionIdentifier (var "f") Nothing [
      Cpp._FunctionIdentifier_simple>>: lambda "name" $ Serialization.cst @@ var "name",
      Cpp._FunctionIdentifier_qualified>>: lambda "q" $ encodeQualifiedIdentifier @@ var "q"]

encodeFunctionSpecifierPrefix :: TTermDefinition (Cpp.FunctionSpecifierPrefix -> Expr)
encodeFunctionSpecifierPrefix = define "encodeFunctionSpecifierPrefix" $
  doc "Convert a function specifier prefix to an expression" $
  lambda "s" $
    cases Cpp._FunctionSpecifierPrefix (var "s") Nothing [
      Cpp._FunctionSpecifierPrefix_inline>>: constant $ Serialization.cst @@ string "inline",
      Cpp._FunctionSpecifierPrefix_virtual>>: constant $ Serialization.cst @@ string "virtual",
      Cpp._FunctionSpecifierPrefix_static>>: constant $ Serialization.cst @@ string "static",
      Cpp._FunctionSpecifierPrefix_explicit>>: constant $ Serialization.cst @@ string "explicit"]

encodeFunctionSpecifierSuffix :: TTermDefinition (Cpp.FunctionSpecifierSuffix -> Expr)
encodeFunctionSpecifierSuffix = define "encodeFunctionSpecifierSuffix" $
  doc "Convert a function specifier suffix to an expression" $
  lambda "s" $
    cases Cpp._FunctionSpecifierSuffix (var "s") Nothing [
      Cpp._FunctionSpecifierSuffix_const>>: constant $ Serialization.cst @@ string "const",
      Cpp._FunctionSpecifierSuffix_noexcept>>: constant $ Serialization.cst @@ string "noexcept",
      Cpp._FunctionSpecifierSuffix_override>>: constant $ Serialization.cst @@ string "override",
      Cpp._FunctionSpecifierSuffix_final>>: constant $ Serialization.cst @@ string "final"]

encodeFunctionType :: TTermDefinition (Cpp.FunctionType -> Expr)
encodeFunctionType = define "encodeFunctionType" $
  doc "Convert a function type to an expression" $
  lambda "ft" $ lets [
    "retType">: project Cpp._FunctionType Cpp._FunctionType_returnType @@ var "ft",
    "params">: project Cpp._FunctionType Cpp._FunctionType_parameters @@ var "ft"] $
    Serialization.spaceSep @@ list [
      encodeTypeExpression @@ var "retType",
      Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeParameter (var "params")))]

encodeGreaterEqualOperation :: TTermDefinition (Cpp.GreaterEqualOperation -> Expr)
encodeGreaterEqualOperation = define "encodeGreaterEqualOperation" $
  doc "Convert a greater-than-or-equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._GreaterEqualOperation Cpp._GreaterEqualOperation_left @@ var "op",
    "right">: project Cpp._GreaterEqualOperation Cpp._GreaterEqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeRelationalExpression @@ var "left",
      Serialization.cst @@ string ">=",
      encodeShiftExpression @@ var "right"]

encodeGreaterOperation :: TTermDefinition (Cpp.GreaterOperation -> Expr)
encodeGreaterOperation = define "encodeGreaterOperation" $
  doc "Convert a greater-than operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._GreaterOperation Cpp._GreaterOperation_left @@ var "op",
    "right">: project Cpp._GreaterOperation Cpp._GreaterOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeRelationalExpression @@ var "left",
      Serialization.cst @@ string ">",
      encodeShiftExpression @@ var "right"]

encodeIfDirective :: TTermDefinition (Cpp.IfDirective -> Expr)
encodeIfDirective = define "encodeIfDirective" $
  doc "Convert an if directive to an expression" $
  lambda "ifd" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#if",
      Serialization.cst @@ (project Cpp._IfDirective Cpp._IfDirective_condition @@ var "ifd")]

encodeIfdefDirective :: TTermDefinition (Cpp.IfdefDirective -> Expr)
encodeIfdefDirective = define "encodeIfdefDirective" $
  doc "Convert an ifdef directive to an expression" $
  lambda "id" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#ifdef",
      Serialization.cst @@ (project Cpp._IfdefDirective Cpp._IfdefDirective_identifier @@ var "id")]

encodeIfndefDirective :: TTermDefinition (Cpp.IfndefDirective -> Expr)
encodeIfndefDirective = define "encodeIfndefDirective" $
  doc "Convert an ifndef directive to an expression" $
  lambda "ind" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#ifndef",
      Serialization.cst @@ (project Cpp._IfndefDirective Cpp._IfndefDirective_identifier @@ var "ind")]

encodeIncludeDirective :: TTermDefinition (Cpp.IncludeDirective -> Expr)
encodeIncludeDirective = define "encodeIncludeDirective" $
  doc "Convert an include directive to an expression" $
  lambda "incl" $ lets [
    "name">: project Cpp._IncludeDirective Cpp._IncludeDirective_name @@ var "incl",
    "isSystem">: project Cpp._IncludeDirective Cpp._IncludeDirective_isSystem @@ var "incl"] $
    Logic.ifElse (var "isSystem")
      (Serialization.cst @@ (Strings.cat $ list [string "#include <", var "name", string ">"]))
      (Serialization.cst @@ (Strings.cat $ list [string "#include \"", var "name", string "\""]))

encodeInclusiveOrExpression :: TTermDefinition (Cpp.InclusiveOrExpression -> Expr)
encodeInclusiveOrExpression = define "encodeInclusiveOrExpression" $
  doc "Convert an inclusive or expression to an expression" $
  lambda "e" $
    cases Cpp._InclusiveOrExpression (var "e") Nothing [
      Cpp._InclusiveOrExpression_exclusiveOr>>: lambda "x" $ encodeExclusiveOrExpression @@ var "x",
      Cpp._InclusiveOrExpression_bitwiseOr>>: lambda "o" $ encodeBitwiseOrOperation @@ var "o"]

encodeIntegerLiteral :: TTermDefinition (Cpp.IntegerLiteral -> Expr)
encodeIntegerLiteral = define "encodeIntegerLiteral" $
  doc "Convert an integer literal to an expression" $
  lambda "i" $
    cases Cpp._IntegerLiteral (var "i") Nothing [
      Cpp._IntegerLiteral_decimal>>: lambda "n" $ Serialization.cst @@ (Literals.showBigint (var "n")),
      Cpp._IntegerLiteral_hexadecimal>>: lambda "h" $ Serialization.cst @@ (Strings.cat2 (string "0x") (var "h")),
      Cpp._IntegerLiteral_octal>>: lambda "o" $ Serialization.cst @@ (Strings.cat2 (string "0") (var "o")),
      Cpp._IntegerLiteral_binary>>: lambda "b" $ Serialization.cst @@ (Strings.cat2 (string "0b") (var "b"))]

encodeIterationStatement :: TTermDefinition (Cpp.IterationStatement -> Expr)
encodeIterationStatement = define "encodeIterationStatement" $
  doc "Convert an iteration statement to an expression" $
  lambda "i" $
    cases Cpp._IterationStatement (var "i") Nothing [
      Cpp._IterationStatement_while>>: lambda "w" $ encodeWhileStatement @@ var "w",
      Cpp._IterationStatement_do>>: lambda "d" $ encodeDoStatement @@ var "d",
      Cpp._IterationStatement_for>>: lambda "f" $ encodeForStatement @@ var "f",
      Cpp._IterationStatement_rangeFor>>: lambda "r" $ encodeRangeForStatement @@ var "r"]

encodeJumpStatement :: TTermDefinition (Cpp.JumpStatement -> Expr)
encodeJumpStatement = define "encodeJumpStatement" $
  doc "Convert a jump statement to an expression" $
  lambda "j" $
    cases Cpp._JumpStatement (var "j") Nothing [
      Cpp._JumpStatement_break>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "break"),
      Cpp._JumpStatement_continue>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "continue"),
      Cpp._JumpStatement_returnValue>>: lambda "e" $
        Serialization.withSemi @@ (Serialization.spaceSep @@ list [Serialization.cst @@ string "return", encodeExpression @@ var "e"]),
      Cpp._JumpStatement_returnVoid>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "return"),
      Cpp._JumpStatement_throw>>: lambda "e" $
        Serialization.withSemi @@ (Serialization.spaceSep @@ list [Serialization.cst @@ string "throw", encodeExpression @@ var "e"])]

encodeLabeledStatement :: TTermDefinition (Cpp.LabeledStatement -> Expr)
encodeLabeledStatement = define "encodeLabeledStatement" $
  doc "Convert a labeled statement to an expression" $
  lambda "ls" $ lets [
    "label">: project Cpp._LabeledStatement Cpp._LabeledStatement_label @@ var "ls",
    "stmt">: project Cpp._LabeledStatement Cpp._LabeledStatement_statement @@ var "ls"] $
    Serialization.newlineSep @@ list [
      Serialization.cst @@ (Strings.cat2 (var "label") (string ":")),
      encodeStatement @@ var "stmt"]

encodeLambdaExpression :: TTermDefinition (Cpp.LambdaExpression -> Expr)
encodeLambdaExpression = define "encodeLambdaExpression" $
  doc "Convert a lambda expression to an expression" $
  lambda "le" $ lets [
    "captures">: project Cpp._LambdaExpression Cpp._LambdaExpression_captures @@ var "le",
    "params">: project Cpp._LambdaExpression Cpp._LambdaExpression_parameters @@ var "le",
    "retType">: project Cpp._LambdaExpression Cpp._LambdaExpression_returnType @@ var "le",
    "body">: project Cpp._LambdaExpression Cpp._LambdaExpression_body @@ var "le"] $
    Serialization.spaceSep @@ list [
      encodeCaptureList @@ var "captures",
      Logic.ifElse (Lists.null (var "params"))
        (Serialization.parens @@ (Serialization.cst @@ string ""))
        (Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeParameter (var "params")))),
      Maybes.maybe
        (Serialization.cst @@ string "")
        (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "->", encodeTypeExpression @@ var "t"])
        (var "retType"),
      encodeCompoundStatement @@ var "body"]

encodeLeftShiftOperation :: TTermDefinition (Cpp.LeftShiftOperation -> Expr)
encodeLeftShiftOperation = define "encodeLeftShiftOperation" $
  doc "Convert a left shift operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LeftShiftOperation Cpp._LeftShiftOperation_left @@ var "op",
    "right">: project Cpp._LeftShiftOperation Cpp._LeftShiftOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeShiftExpression @@ var "left",
      Serialization.cst @@ string "<<",
      encodeAdditiveExpression @@ var "right"]

encodeLessEqualOperation :: TTermDefinition (Cpp.LessEqualOperation -> Expr)
encodeLessEqualOperation = define "encodeLessEqualOperation" $
  doc "Convert a less-than-or-equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LessEqualOperation Cpp._LessEqualOperation_left @@ var "op",
    "right">: project Cpp._LessEqualOperation Cpp._LessEqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeRelationalExpression @@ var "left",
      Serialization.cst @@ string "<=",
      encodeShiftExpression @@ var "right"]

encodeLessOperation :: TTermDefinition (Cpp.LessOperation -> Expr)
encodeLessOperation = define "encodeLessOperation" $
  doc "Convert a less-than operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LessOperation Cpp._LessOperation_left @@ var "op",
    "right">: project Cpp._LessOperation Cpp._LessOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeRelationalExpression @@ var "left",
      Serialization.cst @@ string "<",
      encodeShiftExpression @@ var "right"]

encodeLineDirective :: TTermDefinition (Cpp.LineDirective -> Expr)
encodeLineDirective = define "encodeLineDirective" $
  doc "Convert a line directive to an expression" $
  lambda "ld" $ lets [
    "lineNumber">: project Cpp._LineDirective Cpp._LineDirective_lineNumber @@ var "ld",
    "filename">: project Cpp._LineDirective Cpp._LineDirective_filename @@ var "ld"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [Serialization.cst @@ string "#line", Serialization.cst @@ (Literals.showInt32 (var "lineNumber"))],
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "f" $ list [Serialization.cst @@ (Strings.cat $ list [string "\"", var "f", string "\""])])
        (var "filename")])

encodeLiteral :: TTermDefinition (Cpp.Literal -> Expr)
encodeLiteral = define "encodeLiteral" $
  doc "Convert a literal to an expression" $
  lambda "l" $
    cases Cpp._Literal (var "l") Nothing [
      Cpp._Literal_integer>>: lambda "i" $ encodeIntegerLiteral @@ var "i",
      Cpp._Literal_floating>>: lambda "f" $ Serialization.cst @@ (Literals.showBigfloat (unwrap Cpp._FloatingLiteral @@ var "f")),
      Cpp._Literal_character>>: lambda "c" $
        Serialization.cst @@ (Strings.cat $ list [string "'", unwrap Cpp._CharacterLiteral @@ var "c", string "'"]),
      Cpp._Literal_string>>: lambda "s" $
        Serialization.cst @@ (Strings.cat $ list [string "\"", unwrap Cpp._StringLiteral @@ var "s", string "\""]),
      Cpp._Literal_boolean>>: lambda "b" $ encodeBooleanLiteral @@ var "b",
      Cpp._Literal_null>>: constant $ Serialization.cst @@ string "nullptr"]

encodeLogicalAndExpression :: TTermDefinition (Cpp.LogicalAndExpression -> Expr)
encodeLogicalAndExpression = define "encodeLogicalAndExpression" $
  doc "Convert a logical and expression to an expression" $
  lambda "e" $
    cases Cpp._LogicalAndExpression (var "e") Nothing [
      Cpp._LogicalAndExpression_inclusiveOr>>: lambda "i" $ encodeInclusiveOrExpression @@ var "i",
      Cpp._LogicalAndExpression_logicalAnd>>: lambda "a" $ encodeLogicalAndOperation @@ var "a"]

encodeLogicalAndOperation :: TTermDefinition (Cpp.LogicalAndOperation -> Expr)
encodeLogicalAndOperation = define "encodeLogicalAndOperation" $
  doc "Convert a logical and operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LogicalAndOperation Cpp._LogicalAndOperation_left @@ var "op",
    "right">: project Cpp._LogicalAndOperation Cpp._LogicalAndOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeLogicalAndExpression @@ var "left",
      Serialization.cst @@ string "&&",
      encodeInclusiveOrExpression @@ var "right"]

encodeLogicalOrExpression :: TTermDefinition (Cpp.LogicalOrExpression -> Expr)
encodeLogicalOrExpression = define "encodeLogicalOrExpression" $
  doc "Convert a logical or expression to an expression" $
  lambda "e" $
    cases Cpp._LogicalOrExpression (var "e") Nothing [
      Cpp._LogicalOrExpression_logicalAnd>>: lambda "l" $ encodeLogicalAndExpression @@ var "l",
      Cpp._LogicalOrExpression_logicalOr>>: lambda "o" $ encodeLogicalOrOperation @@ var "o"]

encodeLogicalOrOperation :: TTermDefinition (Cpp.LogicalOrOperation -> Expr)
encodeLogicalOrOperation = define "encodeLogicalOrOperation" $
  doc "Convert a logical or operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LogicalOrOperation Cpp._LogicalOrOperation_left @@ var "op",
    "right">: project Cpp._LogicalOrOperation Cpp._LogicalOrOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeLogicalOrExpression @@ var "left",
      Serialization.cst @@ string "||",
      encodeLogicalAndExpression @@ var "right"]

encodeMap :: TTermDefinition (Cpp.Map -> Expr)
encodeMap = define "encodeMap" $
  doc "Convert a map to an expression" $
  lambda "m" $ lets [
    "keyType">: project Cpp._Map Cpp._Map_keyType @@ var "m",
    "valType">: project Cpp._Map Cpp._Map_valueType @@ var "m",
    "entries">: project Cpp._Map Cpp._Map_entries @@ var "m"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::map<",
      Serialization.commaSep @@ Serialization.inlineStyle @@ list [
        encodeTypeExpression @@ var "keyType",
        encodeTypeExpression @@ var "valType"],
      Serialization.cst @@ string ">",
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ (Lists.map encodeMapEntry (var "entries"))]

encodeMapEntry :: TTermDefinition (Cpp.MapEntry -> Expr)
encodeMapEntry = define "encodeMapEntry" $
  doc "Convert a map entry to an expression" $
  lambda "me" $ lets [
    "key">: project Cpp._MapEntry Cpp._MapEntry_key @@ var "me",
    "val">: project Cpp._MapEntry Cpp._MapEntry_value @@ var "me"] $
    Serialization.spaceSep @@ list [
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ list [encodeExpression @@ var "key"],
      Serialization.cst @@ string "->",
      encodeExpression @@ var "val"]

encodeMemberAccessOperation :: TTermDefinition (Cpp.MemberAccessOperation -> Expr)
encodeMemberAccessOperation = define "encodeMemberAccessOperation" $
  doc "Convert a member access operation to an expression" $
  lambda "mao" $ lets [
    "obj">: project Cpp._MemberAccessOperation Cpp._MemberAccessOperation_object @@ var "mao",
    "member">: project Cpp._MemberAccessOperation Cpp._MemberAccessOperation_member @@ var "mao"] $
    Serialization.noSep @@ list [
      encodePostfixExpression @@ var "obj",
      Serialization.cst @@ string ".",
      Serialization.cst @@ var "member"]

encodeMemberDeclaration :: TTermDefinition (Bool -> Cpp.MemberDeclaration -> Expr)
encodeMemberDeclaration = define "encodeMemberDeclaration" $
  doc "Convert a member declaration to an expression" $
  lambda "commas" $ lambda "m" $
    cases Cpp._MemberDeclaration (var "m") Nothing [
      Cpp._MemberDeclaration_function>>: lambda "f" $ encodeFunctionDeclaration @@ var "f",
      Cpp._MemberDeclaration_variable>>: lambda "v" $ encodeVariableDeclaration @@ var "commas" @@ var "v",
      Cpp._MemberDeclaration_constructor>>: lambda "c" $ encodeConstructorDeclaration @@ var "c",
      Cpp._MemberDeclaration_destructor>>: lambda "d" $ encodeDestructorDeclaration @@ var "d",
      Cpp._MemberDeclaration_nestedClass>>: lambda "c" $ encodeClassDeclaration @@ var "c",
      Cpp._MemberDeclaration_template>>: lambda "t" $ encodeTemplateDeclaration @@ var "t"]

encodeMemberSpecification :: TTermDefinition (Bool -> Cpp.MemberSpecification -> Expr)
encodeMemberSpecification = define "encodeMemberSpecification" $
  doc "Convert a member specification to an expression" $
  lambda "commas" $ lambda "m" $
    cases Cpp._MemberSpecification (var "m") Nothing [
      Cpp._MemberSpecification_accessLabel>>: lambda "a" $
        Serialization.noSep @@ list [encodeAccessSpecifier @@ var "a", Serialization.cst @@ string ":"],
      Cpp._MemberSpecification_member>>: lambda "d" $ encodeMemberDeclaration @@ var "commas" @@ var "d"]

encodeMemInitializer :: TTermDefinition (Cpp.MemInitializer -> Expr)
encodeMemInitializer = define "encodeMemInitializer" $
  doc "Convert a member initializer to an expression" $
  lambda "mi" $ lets [
    "name">: project Cpp._MemInitializer Cpp._MemInitializer_name @@ var "mi",
    "args">: project Cpp._MemInitializer Cpp._MemInitializer_arguments @@ var "mi"] $
    Serialization.noSep @@ list [
      Serialization.cst @@ var "name",
      Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeExpression (var "args")))]

encodeModuloOperation :: TTermDefinition (Cpp.ModuloOperation -> Expr)
encodeModuloOperation = define "encodeModuloOperation" $
  doc "Convert a modulo operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._ModuloOperation Cpp._ModuloOperation_left @@ var "op",
    "right">: project Cpp._ModuloOperation Cpp._ModuloOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeMultiplicativeExpression @@ var "left",
      Serialization.cst @@ string "%",
      encodeUnaryExpression @@ var "right"]

encodeMultiplicativeExpression :: TTermDefinition (Cpp.MultiplicativeExpression -> Expr)
encodeMultiplicativeExpression = define "encodeMultiplicativeExpression" $
  doc "Convert a multiplicative expression to an expression" $
  lambda "e" $
    cases Cpp._MultiplicativeExpression (var "e") Nothing [
      Cpp._MultiplicativeExpression_unary>>: lambda "u" $ encodeUnaryExpression @@ var "u",
      Cpp._MultiplicativeExpression_multiply>>: lambda "m" $ encodeMultiplyOperation @@ var "m",
      Cpp._MultiplicativeExpression_divide>>: lambda "d" $ encodeDivideOperation @@ var "d",
      Cpp._MultiplicativeExpression_modulo>>: lambda "m" $ encodeModuloOperation @@ var "m"]

encodeMultiplyOperation :: TTermDefinition (Cpp.MultiplyOperation -> Expr)
encodeMultiplyOperation = define "encodeMultiplyOperation" $
  doc "Convert a multiply operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._MultiplyOperation Cpp._MultiplyOperation_left @@ var "op",
    "right">: project Cpp._MultiplyOperation Cpp._MultiplyOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeMultiplicativeExpression @@ var "left",
      Serialization.cst @@ string "*",
      encodeUnaryExpression @@ var "right"]

encodeNamespaceDeclaration :: TTermDefinition (Cpp.NamespaceDeclaration -> Expr)
encodeNamespaceDeclaration = define "encodeNamespaceDeclaration" $
  doc "Convert a namespace declaration to an expression" $
  lambda "nd" $ lets [
    "name">: project Cpp._NamespaceDeclaration Cpp._NamespaceDeclaration_name @@ var "nd",
    "decls">: project Cpp._NamespaceDeclaration Cpp._NamespaceDeclaration_declarations @@ var "nd"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ (Strings.cat2 (string "namespace ") (var "name")),
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
        (Serialization.doubleNewlineSep @@ (Lists.map encodeDeclaration (var "decls")))]

encodeNotEqualOperation :: TTermDefinition (Cpp.NotEqualOperation -> Expr)
encodeNotEqualOperation = define "encodeNotEqualOperation" $
  doc "Convert a not-equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._NotEqualOperation Cpp._NotEqualOperation_left @@ var "op",
    "right">: project Cpp._NotEqualOperation Cpp._NotEqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeEqualityExpression @@ var "left",
      Serialization.cst @@ string "!=",
      encodeRelationalExpression @@ var "right"]

encodeOptional :: TTermDefinition (Cpp.Optional -> Expr)
encodeOptional = define "encodeOptional" $
  doc "Convert an optional to an expression" $
  lambda "opt" $ lets [
    "valType">: project Cpp._Optional Cpp._Optional_valueType @@ var "opt",
    "val">: project Cpp._Optional Cpp._Optional_value @@ var "opt"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::optional<",
      encodeTypeExpression @@ var "valType",
      Serialization.cst @@ string ">",
      Maybes.maybe
        (Serialization.cst @@ string "{}")
        (lambda "v" $ Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ list [encodeExpression @@ var "v"])
        (var "val")]

encodeOverloadedLambdas :: TTermDefinition (Cpp.OverloadedLambdas -> Expr)
encodeOverloadedLambdas = define "encodeOverloadedLambdas" $
  doc "Convert overloaded lambdas to an expression" $
  lambda "ol" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "overloaded",
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
        (Serialization.newlineSep @@ (Lists.map encodeLambdaExpression (unwrap Cpp._OverloadedLambdas @@ var "ol")))]

encodeParameter :: TTermDefinition (Cpp.Parameter -> Expr)
encodeParameter = define "encodeParameter" $
  doc "Convert a parameter to an expression" $
  lambda "p" $ lets [
    "typ">: project Cpp._Parameter Cpp._Parameter_type @@ var "p",
    "name">: project Cpp._Parameter Cpp._Parameter_name @@ var "p",
    "unnamed">: project Cpp._Parameter Cpp._Parameter_unnamed @@ var "p",
    "defaultVal">: project Cpp._Parameter Cpp._Parameter_defaultValue @@ var "p",
    "nameExpr">: Serialization.cst @@ (Logic.ifElse (var "unnamed")
      (Strings.cat $ list [string "/*", var "name", string "*/"])
      (var "name"))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [encodeTypeExpression @@ var "typ", var "nameExpr"],
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "expr" $ list [Serialization.cst @@ string "=", encodeExpression @@ var "expr"])
        (var "defaultVal")])

encodePatternMatch :: TTermDefinition (Cpp.PatternMatch -> Expr)
encodePatternMatch = define "encodePatternMatch" $
  doc "Convert a pattern match to an expression" $
  lambda "pm" $ lets [
    "visitor">: project Cpp._PatternMatch Cpp._PatternMatch_visitor @@ var "pm",
    "variant">: project Cpp._PatternMatch Cpp._PatternMatch_variant @@ var "pm"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::visit",
      Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ list [
        encodeVisitor @@ var "visitor",
        encodeExpression @@ var "variant"])]

encodePointerMemberAccessOperation :: TTermDefinition (Cpp.PointerMemberAccessOperation -> Expr)
encodePointerMemberAccessOperation = define "encodePointerMemberAccessOperation" $
  doc "Convert a pointer member access operation to an expression" $
  lambda "pmao" $ lets [
    "ptr">: project Cpp._PointerMemberAccessOperation Cpp._PointerMemberAccessOperation_pointer @@ var "pmao",
    "member">: project Cpp._PointerMemberAccessOperation Cpp._PointerMemberAccessOperation_member @@ var "pmao"] $
    Serialization.noSep @@ list [
      encodePostfixExpression @@ var "ptr",
      Serialization.cst @@ string "->",
      Serialization.cst @@ var "member"]

encodePostfixExpression :: TTermDefinition (Cpp.PostfixExpression -> Expr)
encodePostfixExpression = define "encodePostfixExpression" $
  doc "Convert a postfix expression to an expression" $
  lambda "e" $
    cases Cpp._PostfixExpression (var "e") Nothing [
      Cpp._PostfixExpression_primary>>: lambda "p" $ encodePrimaryExpression @@ var "p",
      Cpp._PostfixExpression_subscript>>: lambda "s" $ encodeSubscriptOperation @@ var "s",
      Cpp._PostfixExpression_functionCall>>: lambda "f" $ encodeFunctionCallOperation @@ var "f",
      Cpp._PostfixExpression_templateFunctionCall>>: lambda "t" $ encodeTemplateFunctionCallOperation @@ var "t",
      Cpp._PostfixExpression_memberAccess>>: lambda "m" $ encodeMemberAccessOperation @@ var "m",
      Cpp._PostfixExpression_pointerMemberAccess>>: lambda "p" $ encodePointerMemberAccessOperation @@ var "p",
      Cpp._PostfixExpression_postIncrement>>: lambda "p" $
        Serialization.noSep @@ list [encodePostfixExpression @@ var "p", Serialization.cst @@ string "++"],
      Cpp._PostfixExpression_postDecrement>>: lambda "p" $
        Serialization.noSep @@ list [encodePostfixExpression @@ var "p", Serialization.cst @@ string "--"]]

encodePragmaDirective :: TTermDefinition (Cpp.PragmaDirective -> Expr)
encodePragmaDirective = define "encodePragmaDirective" $
  doc "Convert a pragma directive to an expression" $
  lambda "pd" $
    Serialization.cst @@ (Strings.cat2 (string "#pragma ") (project Cpp._PragmaDirective Cpp._PragmaDirective_content @@ var "pd"))

encodePreprocessorDirective :: TTermDefinition (Cpp.PreprocessorDirective -> Expr)
encodePreprocessorDirective = define "encodePreprocessorDirective" $
  doc "Convert a preprocessor directive to an expression" $
  lambda "d" $
    cases Cpp._PreprocessorDirective (var "d") Nothing [
      Cpp._PreprocessorDirective_include>>: lambda "i" $ encodeIncludeDirective @@ var "i",
      Cpp._PreprocessorDirective_pragma>>: lambda "p" $ encodePragmaDirective @@ var "p",
      Cpp._PreprocessorDirective_define>>: lambda "d" $ encodeDefineDirective @@ var "d",
      Cpp._PreprocessorDirective_undef>>: lambda "u" $ encodeUndefDirective @@ var "u",
      Cpp._PreprocessorDirective_ifdef>>: lambda "i" $ encodeIfdefDirective @@ var "i",
      Cpp._PreprocessorDirective_ifndef>>: lambda "i" $ encodeIfndefDirective @@ var "i",
      Cpp._PreprocessorDirective_if>>: lambda "i" $ encodeIfDirective @@ var "i",
      Cpp._PreprocessorDirective_elif>>: lambda "e" $ encodeElifDirective @@ var "e",
      Cpp._PreprocessorDirective_else>>: lambda "e" $ encodeElseDirective @@ var "e",
      Cpp._PreprocessorDirective_endif>>: lambda "e" $ encodeEndifDirective @@ var "e",
      Cpp._PreprocessorDirective_line>>: lambda "l" $ encodeLineDirective @@ var "l",
      Cpp._PreprocessorDirective_error>>: lambda "e" $ encodeErrorDirective @@ var "e",
      Cpp._PreprocessorDirective_warning>>: lambda "w" $ encodeWarningDirective @@ var "w"]

encodePrimaryExpression :: TTermDefinition (Cpp.PrimaryExpression -> Expr)
encodePrimaryExpression = define "encodePrimaryExpression" $
  doc "Convert a primary expression to an expression" $
  lambda "e" $
    cases Cpp._PrimaryExpression (var "e") Nothing [
      Cpp._PrimaryExpression_identifier>>: lambda "id" $ Serialization.cst @@ var "id",
      Cpp._PrimaryExpression_literal>>: lambda "l" $ encodeLiteral @@ var "l",
      Cpp._PrimaryExpression_parenthesized>>: lambda "p" $ Serialization.parens @@ (encodeExpression @@ var "p"),
      Cpp._PrimaryExpression_lambda>>: lambda "l" $ encodeLambdaExpression @@ var "l"]

encodeProgram :: TTermDefinition (Cpp.Program -> Expr)
encodeProgram = define "encodeProgram" $
  doc "Convert a program to an expression" $
  lambda "prog" $ lets [
    "preps">: project Cpp._Program Cpp._Program_preprocessorDirectives @@ var "prog",
    "includes">: project Cpp._Program Cpp._Program_includes @@ var "prog",
    "decls">: project Cpp._Program Cpp._Program_declarations @@ var "prog",
    "separate">: lambda "sep" $ lambda "defs" $
      Logic.ifElse (Lists.null (var "defs"))
        nothing
        (just (var "sep" @@ var "defs"))] $
    Serialization.doubleNewlineSep @@ (Maybes.cat $ list [
      var "separate" @@ Serialization.newlineSep @@ (Lists.map encodePreprocessorDirective (var "preps")),
      var "separate" @@ Serialization.newlineSep @@ (Lists.map encodeIncludeDirective (var "includes")),
      var "separate" @@ Serialization.doubleNewlineSep @@ (Lists.map encodeDeclaration (var "decls"))])

encodeQualifiedIdentifier :: TTermDefinition (Cpp.QualifiedIdentifier -> Expr)
encodeQualifiedIdentifier = define "encodeQualifiedIdentifier" $
  doc "Convert a qualified identifier to an expression" $
  lambda "qi" $ lets [
    "ns">: project Cpp._QualifiedIdentifier Cpp._QualifiedIdentifier_namespace @@ var "qi",
    "name">: project Cpp._QualifiedIdentifier Cpp._QualifiedIdentifier_name @@ var "qi"] $
    Serialization.cst @@ (Strings.cat $ list [var "ns", string "::", var "name"])

encodeQualifiedType :: TTermDefinition (Cpp.QualifiedType -> Expr)
encodeQualifiedType = define "encodeQualifiedType" $
  doc "Convert a qualified type to an expression" $
  lambda "qt" $ lets [
    "baseType">: project Cpp._QualifiedType Cpp._QualifiedType_baseType @@ var "qt",
    "qualifier">: project Cpp._QualifiedType Cpp._QualifiedType_qualifier @@ var "qt"] $
    cases Cpp._TypeQualifier (var "qualifier") Nothing [
      Cpp._TypeQualifier_const>>: constant $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "const", encodeTypeExpression @@ var "baseType"],
      Cpp._TypeQualifier_lvalueRef>>: constant $
        Serialization.noSep @@ list [encodeTypeExpression @@ var "baseType", Serialization.cst @@ string "&"],
      Cpp._TypeQualifier_rvalueRef>>: constant $
        Serialization.noSep @@ list [encodeTypeExpression @@ var "baseType", Serialization.cst @@ string "&&"],
      Cpp._TypeQualifier_pointer>>: constant $
        Serialization.noSep @@ list [encodeTypeExpression @@ var "baseType", Serialization.cst @@ string "*"]]

encodeRangeForStatement :: TTermDefinition (Cpp.RangeForStatement -> Expr)
encodeRangeForStatement = define "encodeRangeForStatement" $
  doc "Convert a range-for statement to an expression" $
  lambda "rfs" $ lets [
    "typ">: project Cpp._RangeForStatement Cpp._RangeForStatement_type @@ var "rfs",
    "var">: project Cpp._RangeForStatement Cpp._RangeForStatement_variable @@ var "rfs",
    "range">: project Cpp._RangeForStatement Cpp._RangeForStatement_range @@ var "rfs",
    "body">: project Cpp._RangeForStatement Cpp._RangeForStatement_body @@ var "rfs"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "for",
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          encodeTypeExpression @@ var "typ",
          Serialization.cst @@ var "var",
          Serialization.cst @@ string ":",
          encodeExpression @@ var "range"])],
      encodeStatement @@ var "body"]

encodeRelationalExpression :: TTermDefinition (Cpp.RelationalExpression -> Expr)
encodeRelationalExpression = define "encodeRelationalExpression" $
  doc "Convert a relational expression to an expression" $
  lambda "e" $
    cases Cpp._RelationalExpression (var "e") Nothing [
      Cpp._RelationalExpression_shift>>: lambda "s" $ encodeShiftExpression @@ var "s",
      Cpp._RelationalExpression_less>>: lambda "l" $ encodeLessOperation @@ var "l",
      Cpp._RelationalExpression_greater>>: lambda "g" $ encodeGreaterOperation @@ var "g",
      Cpp._RelationalExpression_lessEqual>>: lambda "le" $ encodeLessEqualOperation @@ var "le",
      Cpp._RelationalExpression_greaterEqual>>: lambda "ge" $ encodeGreaterEqualOperation @@ var "ge"]

encodeRightShiftOperation :: TTermDefinition (Cpp.RightShiftOperation -> Expr)
encodeRightShiftOperation = define "encodeRightShiftOperation" $
  doc "Convert a right shift operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._RightShiftOperation Cpp._RightShiftOperation_left @@ var "op",
    "right">: project Cpp._RightShiftOperation Cpp._RightShiftOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeShiftExpression @@ var "left",
      Serialization.cst @@ string ">>",
      encodeAdditiveExpression @@ var "right"]

encodeSelectionStatement :: TTermDefinition (Cpp.SelectionStatement -> Expr)
encodeSelectionStatement = define "encodeSelectionStatement" $
  doc "Convert a selection statement to an expression" $
  lambda "ss" $ lets [
    "cond">: project Cpp._SelectionStatement Cpp._SelectionStatement_condition @@ var "ss",
    "thenBranch">: project Cpp._SelectionStatement Cpp._SelectionStatement_thenBranch @@ var "ss",
    "elseBranch">: project Cpp._SelectionStatement Cpp._SelectionStatement_elseBranch @@ var "ss"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "if",
        Serialization.parens @@ (encodeExpression @@ var "cond")],
      encodeStatement @@ var "thenBranch",
      Maybes.maybe
        (Serialization.cst @@ string "")
        (lambda "stmt" $ Serialization.newlineSep @@ list [Serialization.cst @@ string "else", encodeStatement @@ var "stmt"])
        (var "elseBranch")]

encodeSet :: TTermDefinition (Cpp.Set -> Expr)
encodeSet = define "encodeSet" $
  doc "Convert a set to an expression" $
  lambda "s" $ lets [
    "elemType">: project Cpp._Set Cpp._Set_elementType @@ var "s",
    "elems">: project Cpp._Set Cpp._Set_elements @@ var "s"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::set<",
      encodeTypeExpression @@ var "elemType",
      Serialization.cst @@ string ">",
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ (Lists.map encodeExpression (var "elems"))]

encodeShiftExpression :: TTermDefinition (Cpp.ShiftExpression -> Expr)
encodeShiftExpression = define "encodeShiftExpression" $
  doc "Convert a shift expression to an expression" $
  lambda "e" $
    cases Cpp._ShiftExpression (var "e") Nothing [
      Cpp._ShiftExpression_additive>>: lambda "a" $ encodeAdditiveExpression @@ var "a",
      Cpp._ShiftExpression_leftShift>>: lambda "ls" $ encodeLeftShiftOperation @@ var "ls",
      Cpp._ShiftExpression_rightShift>>: lambda "rs" $ encodeRightShiftOperation @@ var "rs"]

encodeSizeofExpression :: TTermDefinition (Cpp.SizeofExpression -> Expr)
encodeSizeofExpression = define "encodeSizeofExpression" $
  doc "Convert a sizeof expression to an expression" $
  lambda "se" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "sizeof",
      Serialization.parens @@ (encodeTypeExpression @@ (unwrap Cpp._SizeofExpression @@ var "se"))]

encodeStatement :: TTermDefinition (Cpp.Statement -> Expr)
encodeStatement = define "encodeStatement" $
  doc "Convert a statement to an expression" $
  lambda "s" $
    cases Cpp._Statement (var "s") Nothing [
      Cpp._Statement_labeled>>: lambda "l" $ encodeLabeledStatement @@ var "l",
      Cpp._Statement_compound>>: lambda "c" $ encodeCompoundStatement @@ var "c",
      Cpp._Statement_selection>>: lambda "s" $ encodeSelectionStatement @@ var "s",
      Cpp._Statement_switch>>: lambda "s" $ encodeSwitchStatement @@ var "s",
      Cpp._Statement_iteration>>: lambda "i" $ encodeIterationStatement @@ var "i",
      Cpp._Statement_jump>>: lambda "j" $ encodeJumpStatement @@ var "j",
      Cpp._Statement_declaration>>: lambda "v" $ Serialization.withSemi @@ (encodeVariableDeclaration @@ false @@ var "v"),
      Cpp._Statement_expression>>: lambda "e" $ Serialization.withSemi @@ (encodeExpression @@ var "e")]

encodeSubscriptOperation :: TTermDefinition (Cpp.SubscriptOperation -> Expr)
encodeSubscriptOperation = define "encodeSubscriptOperation" $
  doc "Convert a subscript operation to an expression" $
  lambda "so" $ lets [
    "array">: project Cpp._SubscriptOperation Cpp._SubscriptOperation_array @@ var "so",
    "index">: project Cpp._SubscriptOperation Cpp._SubscriptOperation_index @@ var "so"] $
    Serialization.noSep @@ list [
      encodePostfixExpression @@ var "array",
      Serialization.cst @@ string "[",
      encodeExpression @@ var "index",
      Serialization.cst @@ string "]"]

encodeSubtractOperation :: TTermDefinition (Cpp.SubtractOperation -> Expr)
encodeSubtractOperation = define "encodeSubtractOperation" $
  doc "Convert a subtract operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._SubtractOperation Cpp._SubtractOperation_left @@ var "op",
    "right">: project Cpp._SubtractOperation Cpp._SubtractOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      encodeAdditiveExpression @@ var "left",
      Serialization.cst @@ string "-",
      encodeMultiplicativeExpression @@ var "right"]

encodeSwitchStatement :: TTermDefinition (Cpp.SwitchStatement -> Expr)
encodeSwitchStatement = define "encodeSwitchStatement" $
  doc "Convert a switch statement to an expression" $
  lambda "ss" $ lets [
    "value">: project Cpp._SwitchStatement Cpp._SwitchStatement_value @@ var "ss",
    "cases">: project Cpp._SwitchStatement Cpp._SwitchStatement_cases @@ var "ss"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "switch",
      Serialization.parens @@ (encodeExpression @@ var "value"),
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
        (Serialization.newlineSep @@ (Lists.map encodeCaseStatement (var "cases")))]

encodeTemplateArgument :: TTermDefinition (Cpp.TemplateArgument -> Expr)
encodeTemplateArgument = define "encodeTemplateArgument" $
  doc "Convert a template argument to an expression" $
  lambda "a" $
    cases Cpp._TemplateArgument (var "a") Nothing [
      Cpp._TemplateArgument_type>>: lambda "t" $ encodeTypeExpression @@ var "t",
      Cpp._TemplateArgument_value>>: lambda "e" $ encodeExpression @@ var "e"]

encodeTemplateDeclaration :: TTermDefinition (Cpp.TemplateDeclaration -> Expr)
encodeTemplateDeclaration = define "encodeTemplateDeclaration" $
  doc "Convert a template declaration to an expression" $
  lambda "td" $ lets [
    "inline">: project Cpp._TemplateDeclaration Cpp._TemplateDeclaration_inline @@ var "td",
    "params">: project Cpp._TemplateDeclaration Cpp._TemplateDeclaration_parameters @@ var "td",
    "declaration">: project Cpp._TemplateDeclaration Cpp._TemplateDeclaration_declaration @@ var "td",
    "sep">: Logic.ifElse (var "inline") Serialization.spaceSep Serialization.newlineSep] $
    var "sep" @@ list [
      Serialization.noSep @@ list [
        Serialization.cst @@ string "template",
        Serialization.angleBracesList @@ Serialization.inlineStyle @@
          (Lists.map (lambda "p" $ Serialization.cst @@ var "p") (var "params"))],
      encodeDeclaration @@ var "declaration"]

encodeTemplateFunctionCallOperation :: TTermDefinition (Cpp.TemplateFunctionCallOperation -> Expr)
encodeTemplateFunctionCallOperation = define "encodeTemplateFunctionCallOperation" $
  doc "Convert a template function call operation to an expression" $
  lambda "tfco" $ lets [
    "func">: project Cpp._TemplateFunctionCallOperation Cpp._TemplateFunctionCallOperation_function @@ var "tfco",
    "templateArgs">: project Cpp._TemplateFunctionCallOperation Cpp._TemplateFunctionCallOperation_templateArguments @@ var "tfco",
    "args">: project Cpp._TemplateFunctionCallOperation Cpp._TemplateFunctionCallOperation_arguments @@ var "tfco"] $
    Serialization.noSep @@ list [
      encodePostfixExpression @@ var "func",
      Serialization.angleBracesList @@ Serialization.inlineStyle @@ (Lists.map encodeTemplateArgument (var "templateArgs")),
      Serialization.parens @@ (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map encodeExpression (var "args")))]

encodeTemplateType :: TTermDefinition (Cpp.TemplateType -> Expr)
encodeTemplateType = define "encodeTemplateType" $
  doc "Convert a template type to an expression" $
  lambda "tt" $ lets [
    "name">: project Cpp._TemplateType Cpp._TemplateType_name @@ var "tt",
    "args">: project Cpp._TemplateType Cpp._TemplateType_arguments @@ var "tt"] $
    Serialization.noSep @@ list [
      Serialization.cst @@ var "name",
      Serialization.angleBracesList @@ Serialization.inlineStyle @@ (Lists.map encodeTemplateArgument (var "args"))]

encodeTernaryExpression :: TTermDefinition (Cpp.TernaryExpression -> Expr)
encodeTernaryExpression = define "encodeTernaryExpression" $
  doc "Convert a ternary expression to an expression" $
  lambda "te" $ lets [
    "cond">: project Cpp._TernaryExpression Cpp._TernaryExpression_condition @@ var "te",
    "trueExpr">: project Cpp._TernaryExpression Cpp._TernaryExpression_trueExpr @@ var "te",
    "falseExpr">: project Cpp._TernaryExpression Cpp._TernaryExpression_falseExpr @@ var "te"] $
    Serialization.spaceSep @@ list [
      encodeLogicalOrExpression @@ var "cond",
      Serialization.cst @@ string "?",
      encodeExpression @@ var "trueExpr",
      Serialization.cst @@ string ":",
      encodeConditionalExpression @@ var "falseExpr"]

encodeToCppComments :: TTermDefinition (String -> Bool -> String)
encodeToCppComments = define "toCppComments" $
  doc "Convert a string to a C++ comment" $
  lambda "s" $ lambda "isMultiline" $
    Logic.ifElse (var "isMultiline")
      (Strings.cat $ list [string "/* ", var "s", string " */"])
      (Strings.cat2 (string "// ") (var "s"))

encodeTypeExpression :: TTermDefinition (Cpp.TypeExpression -> Expr)
encodeTypeExpression = define "encodeTypeExpression" $
  doc "Convert a type expression to an expression" $
  lambda "t" $
    cases Cpp._TypeExpression (var "t") Nothing [
      Cpp._TypeExpression_basic>>: lambda "b" $ encodeBasicType @@ var "b",
      Cpp._TypeExpression_qualified>>: lambda "q" $ encodeQualifiedType @@ var "q",
      Cpp._TypeExpression_template>>: lambda "t" $ encodeTemplateType @@ var "t",
      Cpp._TypeExpression_function>>: lambda "f" $ encodeFunctionType @@ var "f",
      Cpp._TypeExpression_auto>>: constant $ Serialization.cst @@ string "auto"]

encodeTypedefDeclaration :: TTermDefinition (Cpp.TypedefDeclaration -> Expr)
encodeTypedefDeclaration = define "encodeTypedefDeclaration" $
  doc "Convert a typedef declaration to an expression" $
  lambda "td" $ lets [
    "name">: project Cpp._TypedefDeclaration Cpp._TypedefDeclaration_name @@ var "td",
    "typ">: project Cpp._TypedefDeclaration Cpp._TypedefDeclaration_type @@ var "td",
    "isUsing">: project Cpp._TypedefDeclaration Cpp._TypedefDeclaration_isUsing @@ var "td"] $
    Logic.ifElse (var "isUsing")
      (Serialization.withSemi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ (Strings.cat2 (string "using ") (var "name")),
        Serialization.cst @@ string "=",
        encodeTypeExpression @@ var "typ"]))
      (Serialization.withSemi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "typedef",
        encodeTypeExpression @@ var "typ",
        Serialization.cst @@ var "name"]))

encodeUnaryExpression :: TTermDefinition (Cpp.UnaryExpression -> Expr)
encodeUnaryExpression = define "encodeUnaryExpression" $
  doc "Convert a unary expression to an expression" $
  lambda "e" $
    cases Cpp._UnaryExpression (var "e") Nothing [
      Cpp._UnaryExpression_postfix>>: lambda "p" $ encodePostfixExpression @@ var "p",
      Cpp._UnaryExpression_unaryOp>>: lambda "o" $ encodeUnaryOperation @@ var "o",
      Cpp._UnaryExpression_sizeof>>: lambda "s" $ encodeSizeofExpression @@ var "s"]

encodeUnaryOperation :: TTermDefinition (Cpp.UnaryOperation -> Expr)
encodeUnaryOperation = define "encodeUnaryOperation" $
  doc "Convert a unary operation to an expression" $
  lambda "uo" $ lets [
    "op">: project Cpp._UnaryOperation Cpp._UnaryOperation_operator @@ var "uo",
    "operand">: project Cpp._UnaryOperation Cpp._UnaryOperation_operand @@ var "uo"] $
    Serialization.spaceSep @@ list [
      encodeUnaryOperator @@ var "op",
      encodeUnaryExpression @@ var "operand"]

encodeUnaryOperator :: TTermDefinition (Cpp.UnaryOperator -> Expr)
encodeUnaryOperator = define "encodeUnaryOperator" $
  doc "Convert a unary operator to an expression" $
  lambda "op" $
    cases Cpp._UnaryOperator (var "op") Nothing [
      Cpp._UnaryOperator_plus>>: constant $ Serialization.cst @@ string "+",
      Cpp._UnaryOperator_minus>>: constant $ Serialization.cst @@ string "-",
      Cpp._UnaryOperator_logicalNot>>: constant $ Serialization.cst @@ string "!",
      Cpp._UnaryOperator_bitwiseNot>>: constant $ Serialization.cst @@ string "~",
      Cpp._UnaryOperator_dereference>>: constant $ Serialization.cst @@ string "*",
      Cpp._UnaryOperator_addressOf>>: constant $ Serialization.cst @@ string "&",
      Cpp._UnaryOperator_preIncrement>>: constant $ Serialization.cst @@ string "++",
      Cpp._UnaryOperator_preDecrement>>: constant $ Serialization.cst @@ string "--"]

encodeUndefDirective :: TTermDefinition (Cpp.UndefDirective -> Expr)
encodeUndefDirective = define "encodeUndefDirective" $
  doc "Convert an undef directive to an expression" $
  lambda "ud" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#undef",
      Serialization.cst @@ (project Cpp._UndefDirective Cpp._UndefDirective_name @@ var "ud")]

encodeVariableDeclaration :: TTermDefinition (Bool -> Cpp.VariableDeclaration -> Expr)
encodeVariableDeclaration = define "encodeVariableDeclaration" $
  doc "Convert a variable declaration to an expression" $
  lambda "commas" $ lambda "vd" $ lets [
    "typ">: project Cpp._VariableDeclaration Cpp._VariableDeclaration_type @@ var "vd",
    "name">: project Cpp._VariableDeclaration Cpp._VariableDeclaration_name @@ var "vd",
    "init">: project Cpp._VariableDeclaration Cpp._VariableDeclaration_initializer @@ var "vd",
    "isAuto">: project Cpp._VariableDeclaration Cpp._VariableDeclaration_isAuto @@ var "vd",
    "terminator">: Logic.ifElse (var "commas") Serialization.withComma Serialization.withSemi] $
    var "terminator" @@ (Serialization.spaceSep @@ (Lists.concat $ list [
      Logic.ifElse (var "isAuto")
        (list [Serialization.cst @@ string "auto"])
        (Maybes.maybe
          (list ([] :: [TTerm Expr]))
          (lambda "t" $ list [encodeTypeExpression @@ var "t"])
          (var "typ")),
      list [Serialization.cst @@ var "name"],
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "expr" $ list [Serialization.cst @@ string "=", encodeExpression @@ var "expr"])
        (var "init")]))

encodeVector :: TTermDefinition (Cpp.Vector -> Expr)
encodeVector = define "encodeVector" $
  doc "Convert a vector to an expression" $
  lambda "v" $ lets [
    "elemType">: project Cpp._Vector Cpp._Vector_elementType @@ var "v",
    "elems">: project Cpp._Vector Cpp._Vector_elements @@ var "v"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::vector<",
      encodeTypeExpression @@ var "elemType",
      Serialization.cst @@ string ">",
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ (Lists.map encodeExpression (var "elems"))]

encodeVisitor :: TTermDefinition (Cpp.Visitor -> Expr)
encodeVisitor = define "encodeVisitor" $
  doc "Convert a visitor to an expression" $
  lambda "v" $
    cases Cpp._Visitor (var "v") Nothing [
      Cpp._Visitor_lambda>>: lambda "l" $ encodeLambdaExpression @@ var "l",
      Cpp._Visitor_overloaded>>: lambda "o" $ encodeOverloadedLambdas @@ var "o"]

encodeWarningDirective :: TTermDefinition (Cpp.WarningDirective -> Expr)
encodeWarningDirective = define "encodeWarningDirective" $
  doc "Convert a warning directive to an expression" $
  lambda "wd" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#warning",
      Serialization.cst @@ (project Cpp._WarningDirective Cpp._WarningDirective_message @@ var "wd")]

encodeWhileStatement :: TTermDefinition (Cpp.WhileStatement -> Expr)
encodeWhileStatement = define "encodeWhileStatement" $
  doc "Convert a while statement to an expression" $
  lambda "ws" $ lets [
    "cond">: project Cpp._WhileStatement Cpp._WhileStatement_condition @@ var "ws",
    "body">: project Cpp._WhileStatement Cpp._WhileStatement_body @@ var "ws"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "while",
        Serialization.parens @@ (encodeExpression @@ var "cond")],
      encodeStatement @@ var "body"]

