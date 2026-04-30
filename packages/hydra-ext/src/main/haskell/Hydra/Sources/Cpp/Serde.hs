-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Sources.Cpp.Serde where

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
import qualified Hydra.Cpp.Syntax as Cpp
import qualified Hydra.Sources.Cpp.Syntax as CppSyntax


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.cpp.serde"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Serialization.ns],
            moduleTypeDependencies = (CppSyntax.ns:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "Serialization functions for converting C++ AST to abstract expressions"}
  where
    definitions = [
      toDefinition accessSpecifierToExpr,
      toDefinition addOperationToExpr,
      toDefinition additiveExpressionToExpr,
      toDefinition andExpressionToExpr,
      toDefinition assignmentExpressionToExpr,
      toDefinition assignmentOperatorToExpr,
      toDefinition baseSpecifierToExpr,
      toDefinition basicTypeToExpr,
      toDefinition bitwiseAndOperationToExpr,
      toDefinition bitwiseOrOperationToExpr,
      toDefinition bitwiseXorOperationToExpr,
      toDefinition booleanLiteralToExpr,
      toDefinition captureToExpr,
      toDefinition captureListToExpr,
      toDefinition caseStatementToExpr,
      toDefinition caseValueToExpr,
      toDefinition classBodyToExpr,
      toDefinition classDeclarationToExpr,
      toDefinition classKeyToExpr,
      toDefinition classSpecifierToExpr,
      toDefinition commaExpressionToExpr,
      toDefinition commentToExpr,
      toDefinition compoundStatementToExpr,
      toDefinition conditionalExpressionToExpr,
      toDefinition constructorDeclarationToExpr,
      toDefinition declarationToExpr,
      toDefinition defineDirectiveToExpr,
      toDefinition destructorDeclarationToExpr,
      toDefinition divideOperationToExpr,
      toDefinition doStatementToExpr,
      toDefinition elifDirectiveToExpr,
      toDefinition elseDirectiveToExpr,
      toDefinition endifDirectiveToExpr,
      toDefinition equalOperationToExpr,
      toDefinition equalityExpressionToExpr,
      toDefinition errorDirectiveToExpr,
      toDefinition exclusiveOrExpressionToExpr,
      toDefinition explicitAssignmentToExpr,
      toDefinition expressionToExpr,
      toDefinition forInitToExpr,
      toDefinition forStatementToExpr,
      toDefinition functionApplicationToExpr,
      toDefinition functionBodyToExpr,
      toDefinition functionCallOperationToExpr,
      toDefinition functionDeclarationToExpr,
      toDefinition functionIdentifierToExpr,
      toDefinition functionSpecifierPrefixToExpr,
      toDefinition functionSpecifierSuffixToExpr,
      toDefinition functionTypeToExpr,
      toDefinition greaterEqualOperationToExpr,
      toDefinition greaterOperationToExpr,
      toDefinition ifDirectiveToExpr,
      toDefinition ifdefDirectiveToExpr,
      toDefinition ifndefDirectiveToExpr,
      toDefinition includeDirectiveToExpr,
      toDefinition inclusiveOrExpressionToExpr,
      toDefinition integerLiteralToExpr,
      toDefinition iterationStatementToExpr,
      toDefinition jumpStatementToExpr,
      toDefinition labeledStatementToExpr,
      toDefinition lambdaExpressionToExpr,
      toDefinition leftShiftOperationToExpr,
      toDefinition lessEqualOperationToExpr,
      toDefinition lessOperationToExpr,
      toDefinition lineDirectiveToExpr,
      toDefinition literalToExpr,
      toDefinition logicalAndExpressionToExpr,
      toDefinition logicalAndOperationToExpr,
      toDefinition logicalOrExpressionToExpr,
      toDefinition logicalOrOperationToExpr,
      toDefinition mapToExpr,
      toDefinition mapEntryToExpr,
      toDefinition memInitializerToExpr,
      toDefinition memberAccessOperationToExpr,
      toDefinition memberDeclarationToExpr,
      toDefinition memberSpecificationToExpr,
      toDefinition moduloOperationToExpr,
      toDefinition multiplicativeExpressionToExpr,
      toDefinition multiplyOperationToExpr,
      toDefinition namespaceDeclarationToExpr,
      toDefinition notEqualOperationToExpr,
      toDefinition optionalToExpr,
      toDefinition overloadedLambdasToExpr,
      toDefinition parameterToExpr,
      toDefinition patternMatchToExpr,
      toDefinition pointerMemberAccessOperationToExpr,
      toDefinition postfixExpressionToExpr,
      toDefinition pragmaDirectiveToExpr,
      toDefinition preprocessorDirectiveToExpr,
      toDefinition primaryExpressionToExpr,
      toDefinition programToExpr,
      toDefinition qualifiedIdentifierToExpr,
      toDefinition qualifiedTypeToExpr,
      toDefinition rangeForStatementToExpr,
      toDefinition relationalExpressionToExpr,
      toDefinition rightShiftOperationToExpr,
      toDefinition selectionStatementToExpr,
      toDefinition setToExpr,
      toDefinition shiftExpressionToExpr,
      toDefinition sizeofExpressionToExpr,
      toDefinition statementToExpr,
      toDefinition subscriptOperationToExpr,
      toDefinition subtractOperationToExpr,
      toDefinition switchStatementToExpr,
      toDefinition templateArgumentToExpr,
      toDefinition templateDeclarationToExpr,
      toDefinition templateFunctionCallOperationToExpr,
      toDefinition templateTypeToExpr,
      toDefinition ternaryExpressionToExpr,
      toDefinition toCppCommentsToExpr,
      toDefinition typeExpressionToExpr,
      toDefinition typedefDeclarationToExpr,
      toDefinition unaryExpressionToExpr,
      toDefinition unaryOperationToExpr,
      toDefinition unaryOperatorToExpr,
      toDefinition undefDirectiveToExpr,
      toDefinition variableDeclarationToExpr,
      toDefinition vectorToExpr,
      toDefinition visitorToExpr,
      toDefinition warningDirectiveToExpr,
      toDefinition whileStatementToExpr]


accessSpecifierToExpr :: TTermDefinition (Cpp.AccessSpecifier -> Expr)
accessSpecifierToExpr = define "accessSpecifierToExpr" $
  doc "Convert an access specifier to an expression" $
  lambda "a" $
    cases Cpp._AccessSpecifier (var "a") Nothing [
      Cpp._AccessSpecifier_public>>: constant $ Serialization.cst @@ string "public",
      Cpp._AccessSpecifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Cpp._AccessSpecifier_private>>: constant $ Serialization.cst @@ string "private",
      Cpp._AccessSpecifier_none>>: constant $ Serialization.cst @@ string ""]

addOperationToExpr :: TTermDefinition (Cpp.AddOperation -> Expr)
addOperationToExpr = define "addOperationToExpr" $
  doc "Convert an add operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._AddOperation Cpp._AddOperation_left @@ var "op",
    "right">: project Cpp._AddOperation Cpp._AddOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      additiveExpressionToExpr @@ var "left",
      Serialization.cst @@ string "+",
      multiplicativeExpressionToExpr @@ var "right"]

additiveExpressionToExpr :: TTermDefinition (Cpp.AdditiveExpression -> Expr)
additiveExpressionToExpr = define "additiveExpressionToExpr" $
  doc "Convert an additive expression to an expression" $
  lambda "e" $
    cases Cpp._AdditiveExpression (var "e") Nothing [
      Cpp._AdditiveExpression_multiplicative>>: lambda "m" $ multiplicativeExpressionToExpr @@ var "m",
      Cpp._AdditiveExpression_add>>: lambda "a" $ addOperationToExpr @@ var "a",
      Cpp._AdditiveExpression_subtract>>: lambda "s" $ subtractOperationToExpr @@ var "s"]

andExpressionToExpr :: TTermDefinition (Cpp.AndExpression -> Expr)
andExpressionToExpr = define "andExpressionToExpr" $
  doc "Convert an and expression to an expression" $
  lambda "e" $
    cases Cpp._AndExpression (var "e") Nothing [
      Cpp._AndExpression_equality>>: lambda "eq" $ equalityExpressionToExpr @@ var "eq",
      Cpp._AndExpression_bitwiseAnd>>: lambda "a" $ bitwiseAndOperationToExpr @@ var "a"]

assignmentExpressionToExpr :: TTermDefinition (Cpp.AssignmentExpression -> Expr)
assignmentExpressionToExpr = define "assignmentExpressionToExpr" $
  doc "Convert an assignment expression to an expression" $
  lambda "a" $
    cases Cpp._AssignmentExpression (var "a") Nothing [
      Cpp._AssignmentExpression_conditional>>: lambda "c" $ conditionalExpressionToExpr @@ var "c",
      Cpp._AssignmentExpression_assignment>>: lambda "e" $ explicitAssignmentToExpr @@ var "e"]

assignmentOperatorToExpr :: TTermDefinition (Cpp.AssignmentOperator -> Expr)
assignmentOperatorToExpr = define "assignmentOperatorToExpr" $
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

baseSpecifierToExpr :: TTermDefinition (Cpp.BaseSpecifier -> Expr)
baseSpecifierToExpr = define "baseSpecifierToExpr" $
  doc "Convert a base specifier to an expression" $
  lambda "bs" $ lets [
    "access">: project Cpp._BaseSpecifier Cpp._BaseSpecifier_access @@ var "bs",
    "name">: project Cpp._BaseSpecifier Cpp._BaseSpecifier_name @@ var "bs"] $
    Serialization.spaceSep @@ list [accessSpecifierToExpr @@ var "access", Serialization.cst @@ var "name"]

basicTypeToExpr :: TTermDefinition (Cpp.BasicType -> Expr)
basicTypeToExpr = define "basicTypeToExpr" $
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

bitwiseAndOperationToExpr :: TTermDefinition (Cpp.BitwiseAndOperation -> Expr)
bitwiseAndOperationToExpr = define "bitwiseAndOperationToExpr" $
  doc "Convert a bitwise and operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._BitwiseAndOperation Cpp._BitwiseAndOperation_left @@ var "op",
    "right">: project Cpp._BitwiseAndOperation Cpp._BitwiseAndOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      andExpressionToExpr @@ var "left",
      Serialization.cst @@ string "&",
      equalityExpressionToExpr @@ var "right"]

bitwiseOrOperationToExpr :: TTermDefinition (Cpp.BitwiseOrOperation -> Expr)
bitwiseOrOperationToExpr = define "bitwiseOrOperationToExpr" $
  doc "Convert a bitwise or operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._BitwiseOrOperation Cpp._BitwiseOrOperation_left @@ var "op",
    "right">: project Cpp._BitwiseOrOperation Cpp._BitwiseOrOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      inclusiveOrExpressionToExpr @@ var "left",
      Serialization.cst @@ string "|",
      exclusiveOrExpressionToExpr @@ var "right"]

bitwiseXorOperationToExpr :: TTermDefinition (Cpp.BitwiseXorOperation -> Expr)
bitwiseXorOperationToExpr = define "bitwiseXorOperationToExpr" $
  doc "Convert a bitwise xor operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._BitwiseXorOperation Cpp._BitwiseXorOperation_left @@ var "op",
    "right">: project Cpp._BitwiseXorOperation Cpp._BitwiseXorOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      exclusiveOrExpressionToExpr @@ var "left",
      Serialization.cst @@ string "^",
      andExpressionToExpr @@ var "right"]

booleanLiteralToExpr :: TTermDefinition (Cpp.BooleanLiteral -> Expr)
booleanLiteralToExpr = define "booleanLiteralToExpr" $
  doc "Convert a boolean literal to an expression" $
  lambda "bl" $
    Logic.ifElse (unwrap Cpp._BooleanLiteral @@ var "bl")
      (Serialization.cst @@ string "true")
      (Serialization.cst @@ string "false")

captureToExpr :: TTermDefinition (Cpp.Capture -> Expr)
captureToExpr = define "captureToExpr" $
  doc "Convert a capture to an expression" $
  lambda "cap" $ lets [
    "name">: project Cpp._Capture Cpp._Capture_name @@ var "cap",
    "byRef">: project Cpp._Capture Cpp._Capture_byReference @@ var "cap"] $
    Logic.ifElse (var "byRef")
      (Serialization.cst @@ (Strings.cat2 (string "&") (var "name")))
      (Serialization.cst @@ var "name")

captureListToExpr :: TTermDefinition (Cpp.CaptureList -> Expr)
captureListToExpr = define "captureListToExpr" $
  doc "Convert a capture list to an expression" $
  lambda "cl" $
    cases Cpp._CaptureList (var "cl") Nothing [
      Cpp._CaptureList_captureByValue>>: constant $ Serialization.cst @@ string "[=]",
      Cpp._CaptureList_captures>>: lambda "cs" $
        Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map captureToExpr (var "cs"))]

caseStatementToExpr :: TTermDefinition (Cpp.CaseStatement -> Expr)
caseStatementToExpr = define "caseStatementToExpr" $
  doc "Convert a case statement to an expression" $
  lambda "stmt" $
    cases Cpp._CaseStatement (var "stmt") Nothing [
      Cpp._CaseStatement_case>>: lambda "cv" $ caseValueToExpr @@ var "cv",
      Cpp._CaseStatement_default>>: lambda "s" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "default:",
          statementToExpr @@ var "s"]]

caseValueToExpr :: TTermDefinition (Cpp.CaseValue -> Expr)
caseValueToExpr = define "caseValueToExpr" $
  doc "Convert a case value to an expression" $
  lambda "cv" $ lets [
    "value">: project Cpp._CaseValue Cpp._CaseValue_value @@ var "cv",
    "statement">: project Cpp._CaseValue Cpp._CaseValue_statement @@ var "cv"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "case",
      Serialization.noSep @@ list [expressionToExpr @@ var "value", Serialization.cst @@ string ":"],
      statementToExpr @@ var "statement"]

classBodyToExpr :: TTermDefinition (Bool -> Cpp.ClassBody -> Expr)
classBodyToExpr = define "classBodyToExpr" $
  doc "Convert a class body to an expression" $
  lambda "commas" $ lambda "cb" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.doubleNewlineSep @@ (Lists.map (memberSpecificationToExpr @@ var "commas") (unwrap Cpp._ClassBody @@ var "cb")))

classDeclarationToExpr :: TTermDefinition (Cpp.ClassDeclaration -> Expr)
classDeclarationToExpr = define "classDeclarationToExpr" $
  doc "Convert a class declaration to an expression" $
  lambda "cd" $ lets [
    "spec">: project Cpp._ClassDeclaration Cpp._ClassDeclaration_specifier @@ var "cd",
    "mbody">: project Cpp._ClassDeclaration Cpp._ClassDeclaration_body @@ var "cd",
    "key">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_key @@ var "spec",
    "isEnum">: Logic.or
      (Equality.equal (var "key") (inject Cpp._ClassKey Cpp._ClassKey_enum unit))
      (Equality.equal (var "key") (inject Cpp._ClassKey Cpp._ClassKey_enumClass unit))] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ (Maybes.cat $ list [
      just (classSpecifierToExpr @@ var "spec"),
      Maybes.map (lambda "body" $ classBodyToExpr @@ var "isEnum" @@ var "body") (var "mbody")]))

classKeyToExpr :: TTermDefinition (Cpp.ClassKey -> Expr)
classKeyToExpr = define "classKeyToExpr" $
  doc "Convert a class key to an expression" $
  lambda "k" $
    cases Cpp._ClassKey (var "k") Nothing [
      Cpp._ClassKey_class>>: constant $ Serialization.cst @@ string "class",
      Cpp._ClassKey_enum>>: constant $ Serialization.cst @@ string "enum",
      Cpp._ClassKey_enumClass>>: constant $ Serialization.cst @@ string "enum class",
      Cpp._ClassKey_struct>>: constant $ Serialization.cst @@ string "struct"]

classSpecifierToExpr :: TTermDefinition (Cpp.ClassSpecifier -> Expr)
classSpecifierToExpr = define "classSpecifierToExpr" $
  doc "Convert a class specifier to an expression" $
  lambda "cs" $ lets [
    "key">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_key @@ var "cs",
    "name">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_name @@ var "cs",
    "inheritance">: project Cpp._ClassSpecifier Cpp._ClassSpecifier_inheritance @@ var "cs"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [classKeyToExpr @@ var "key", Serialization.cst @@ var "name"],
      Logic.ifElse (Lists.null (var "inheritance"))
        (list ([] :: [TTerm Expr]))
        (list [Serialization.cst @@ string ":",
          Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map baseSpecifierToExpr (var "inheritance"))])])

commaExpressionToExpr :: TTermDefinition (Cpp.CommaExpression -> Expr)
commaExpressionToExpr = define "commaExpressionToExpr" $
  doc "Convert a comma expression to an expression" $
  lambda "ce" $ lets [
    "left">: project Cpp._CommaExpression Cpp._CommaExpression_left @@ var "ce",
    "right">: project Cpp._CommaExpression Cpp._CommaExpression_right @@ var "ce"] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "left",
      Serialization.cst @@ string ",",
      assignmentExpressionToExpr @@ var "right"]

commentToExpr :: TTermDefinition (Cpp.Comment -> Expr)
commentToExpr = define "commentToExpr" $
  doc "Convert a comment to an expression" $
  lambda "c" $ lets [
    "text">: project Cpp._Comment Cpp._Comment_text @@ var "c",
    "isMultiline">: project Cpp._Comment Cpp._Comment_isMultiline @@ var "c"] $
    Serialization.cst @@ (toCppCommentsToExpr @@ var "text" @@ var "isMultiline")

compoundStatementToExpr :: TTermDefinition (Cpp.CompoundStatement -> Expr)
compoundStatementToExpr = define "compoundStatementToExpr" $
  doc "Convert a compound statement to an expression" $
  lambda "cs" $
    Serialization.curlyBracesList @@ (just (string "")) @@ Serialization.fullBlockStyle @@
      (Lists.map statementToExpr (unwrap Cpp._CompoundStatement @@ var "cs"))

conditionalExpressionToExpr :: TTermDefinition (Cpp.ConditionalExpression -> Expr)
conditionalExpressionToExpr = define "conditionalExpressionToExpr" $
  doc "Convert a conditional expression to an expression" $
  lambda "c" $
    cases Cpp._ConditionalExpression (var "c") Nothing [
      Cpp._ConditionalExpression_logicalOr>>: lambda "l" $ logicalOrExpressionToExpr @@ var "l",
      Cpp._ConditionalExpression_ternary>>: lambda "t" $ ternaryExpressionToExpr @@ var "t"]

constructorDeclarationToExpr :: TTermDefinition (Cpp.ConstructorDeclaration -> Expr)
constructorDeclarationToExpr = define "constructorDeclarationToExpr" $
  doc "Convert a constructor declaration to an expression" $
  lambda "cd" $ lets [
    "name">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_name @@ var "cd",
    "params">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_parameters @@ var "cd",
    "inits">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_initializers @@ var "cd",
    "body">: project Cpp._ConstructorDeclaration Cpp._ConstructorDeclaration_body @@ var "cd"] $
    Serialization.spaceSep @@ (Maybes.cat $ list [
      just (Serialization.noSep @@ list [
        Serialization.cst @@ var "name",
        Serialization.parenListAdaptive @@ (Lists.map parameterToExpr (var "params"))]),
      Logic.ifElse (Lists.null (var "inits"))
        nothing
        (just (Serialization.spaceSep @@ list [
          Serialization.cst @@ string ":",
          Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map memInitializerToExpr (var "inits"))])),
      just (functionBodyToExpr @@ var "body")])

declarationToExpr :: TTermDefinition (Cpp.Declaration -> Expr)
declarationToExpr = define "declarationToExpr" $
  doc "Convert a declaration to an expression" $
  lambda "d" $
    cases Cpp._Declaration (var "d") Nothing [
      Cpp._Declaration_preprocessor>>: lambda "p" $ preprocessorDirectiveToExpr @@ var "p",
      Cpp._Declaration_class>>: lambda "c" $ classDeclarationToExpr @@ var "c",
      Cpp._Declaration_function>>: lambda "f" $ functionDeclarationToExpr @@ var "f",
      Cpp._Declaration_variable>>: lambda "v" $ variableDeclarationToExpr @@ false @@ var "v",
      Cpp._Declaration_typedef>>: lambda "t" $ typedefDeclarationToExpr @@ var "t",
      Cpp._Declaration_namespace>>: lambda "n" $ namespaceDeclarationToExpr @@ var "n",
      Cpp._Declaration_template>>: lambda "t" $ templateDeclarationToExpr @@ var "t"]

defineDirectiveToExpr :: TTermDefinition (Cpp.DefineDirective -> Expr)
defineDirectiveToExpr = define "defineDirectiveToExpr" $
  doc "Convert a define directive to an expression" $
  lambda "dd" $ lets [
    "name">: project Cpp._DefineDirective Cpp._DefineDirective_name @@ var "dd",
    "params">: project Cpp._DefineDirective Cpp._DefineDirective_parameters @@ var "dd",
    "replacement">: project Cpp._DefineDirective Cpp._DefineDirective_replacement @@ var "dd"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [Serialization.cst @@ string "#define", Serialization.cst @@ var "name"],
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "ps" $ list [Serialization.parenListAdaptive @@ (Lists.map (lambda "p" $ Serialization.cst @@ var "p") (var "ps"))])
        (var "params"),
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "r" $ list [Serialization.cst @@ var "r"])
        (var "replacement")])

destructorDeclarationToExpr :: TTermDefinition (Cpp.DestructorDeclaration -> Expr)
destructorDeclarationToExpr = define "destructorDeclarationToExpr" $
  doc "Convert a destructor declaration to an expression" $
  lambda "dd" $ lets [
    "prefixSpecs">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_prefixSpecifiers @@ var "dd",
    "name">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_name @@ var "dd",
    "suffixSpecs">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_suffixSpecifiers @@ var "dd",
    "body">: project Cpp._DestructorDeclaration Cpp._DestructorDeclaration_body @@ var "dd"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      Lists.map functionSpecifierPrefixToExpr (var "prefixSpecs"),
      list [Serialization.noSep @@ list [
        Serialization.cst @@ (Strings.cat2 (string "~") (var "name")),
        Serialization.parens @@ (Serialization.cst @@ string "")]],
      Lists.map functionSpecifierSuffixToExpr (var "suffixSpecs"),
      list [functionBodyToExpr @@ var "body"]])

divideOperationToExpr :: TTermDefinition (Cpp.DivideOperation -> Expr)
divideOperationToExpr = define "divideOperationToExpr" $
  doc "Convert a divide operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._DivideOperation Cpp._DivideOperation_left @@ var "op",
    "right">: project Cpp._DivideOperation Cpp._DivideOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      multiplicativeExpressionToExpr @@ var "left",
      Serialization.cst @@ string "/",
      unaryExpressionToExpr @@ var "right"]

doStatementToExpr :: TTermDefinition (Cpp.DoStatement -> Expr)
doStatementToExpr = define "doStatementToExpr" $
  doc "Convert a do statement to an expression" $
  lambda "ds" $ lets [
    "body">: project Cpp._DoStatement Cpp._DoStatement_body @@ var "ds",
    "cond">: project Cpp._DoStatement Cpp._DoStatement_condition @@ var "ds"] $
    Serialization.newlineSep @@ list [
      Serialization.cst @@ string "do",
      statementToExpr @@ var "body",
      Serialization.withSemi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "while",
        Serialization.parens @@ (expressionToExpr @@ var "cond")])]

elifDirectiveToExpr :: TTermDefinition (Cpp.ElifDirective -> Expr)
elifDirectiveToExpr = define "elifDirectiveToExpr" $
  doc "Convert an elif directive to an expression" $
  lambda "ed" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#elif",
      Serialization.cst @@ (project Cpp._ElifDirective Cpp._ElifDirective_condition @@ var "ed")]

elseDirectiveToExpr :: TTermDefinition (Cpp.ElseDirective -> Expr)
elseDirectiveToExpr = define "elseDirectiveToExpr" $
  doc "Convert an else directive to an expression" $
  lambda "ed" $ Serialization.cst @@ string "#else"

endifDirectiveToExpr :: TTermDefinition (Cpp.EndifDirective -> Expr)
endifDirectiveToExpr = define "endifDirectiveToExpr" $
  doc "Convert an endif directive to an expression" $
  lambda "ed" $ Serialization.cst @@ string "#endif"

equalOperationToExpr :: TTermDefinition (Cpp.EqualOperation -> Expr)
equalOperationToExpr = define "equalOperationToExpr" $
  doc "Convert an equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._EqualOperation Cpp._EqualOperation_left @@ var "op",
    "right">: project Cpp._EqualOperation Cpp._EqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      equalityExpressionToExpr @@ var "left",
      Serialization.cst @@ string "==",
      relationalExpressionToExpr @@ var "right"]

equalityExpressionToExpr :: TTermDefinition (Cpp.EqualityExpression -> Expr)
equalityExpressionToExpr = define "equalityExpressionToExpr" $
  doc "Convert an equality expression to an expression" $
  lambda "e" $
    cases Cpp._EqualityExpression (var "e") Nothing [
      Cpp._EqualityExpression_relational>>: lambda "r" $ relationalExpressionToExpr @@ var "r",
      Cpp._EqualityExpression_equal>>: lambda "eq" $ equalOperationToExpr @@ var "eq",
      Cpp._EqualityExpression_notEqual>>: lambda "ne" $ notEqualOperationToExpr @@ var "ne"]

errorDirectiveToExpr :: TTermDefinition (Cpp.ErrorDirective -> Expr)
errorDirectiveToExpr = define "errorDirectiveToExpr" $
  doc "Convert an error directive to an expression" $
  lambda "ed" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#error",
      Serialization.cst @@ (project Cpp._ErrorDirective Cpp._ErrorDirective_message @@ var "ed")]

exclusiveOrExpressionToExpr :: TTermDefinition (Cpp.ExclusiveOrExpression -> Expr)
exclusiveOrExpressionToExpr = define "exclusiveOrExpressionToExpr" $
  doc "Convert an exclusive or expression to an expression" $
  lambda "e" $
    cases Cpp._ExclusiveOrExpression (var "e") Nothing [
      Cpp._ExclusiveOrExpression_and>>: lambda "a" $ andExpressionToExpr @@ var "a",
      Cpp._ExclusiveOrExpression_bitwiseXor>>: lambda "x" $ bitwiseXorOperationToExpr @@ var "x"]

explicitAssignmentToExpr :: TTermDefinition (Cpp.ExplicitAssignment -> Expr)
explicitAssignmentToExpr = define "explicitAssignmentToExpr" $
  doc "Convert an explicit assignment to an expression" $
  lambda "ea" $ lets [
    "left">: project Cpp._ExplicitAssignment Cpp._ExplicitAssignment_left @@ var "ea",
    "op">: project Cpp._ExplicitAssignment Cpp._ExplicitAssignment_op @@ var "ea",
    "right">: project Cpp._ExplicitAssignment Cpp._ExplicitAssignment_right @@ var "ea"] $
    Serialization.spaceSep @@ list [
      logicalOrExpressionToExpr @@ var "left",
      assignmentOperatorToExpr @@ var "op",
      assignmentExpressionToExpr @@ var "right"]

expressionToExpr :: TTermDefinition (Cpp.Expression -> Expr)
expressionToExpr = define "expressionToExpr" $
  doc "Convert an expression to an expression" $
  lambda "e" $
    cases Cpp._Expression (var "e") Nothing [
      Cpp._Expression_assignment>>: lambda "a" $ assignmentExpressionToExpr @@ var "a",
      Cpp._Expression_comma>>: lambda "c" $ commaExpressionToExpr @@ var "c"]

forInitToExpr :: TTermDefinition (Cpp.ForInit -> Expr)
forInitToExpr = define "forInitToExpr" $
  doc "Convert a for-init to an expression" $
  lambda "i" $
    cases Cpp._ForInit (var "i") Nothing [
      Cpp._ForInit_expression>>: lambda "e" $ expressionToExpr @@ var "e",
      Cpp._ForInit_declaration>>: lambda "d" $ variableDeclarationToExpr @@ false @@ var "d",
      Cpp._ForInit_empty>>: constant $ Serialization.cst @@ string ""]

forStatementToExpr :: TTermDefinition (Cpp.ForStatement -> Expr)
forStatementToExpr = define "forStatementToExpr" $
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
          forInitToExpr @@ var "init",
          Serialization.cst @@ string ";",
          expressionToExpr @@ var "cond",
          Serialization.cst @@ string ";",
          expressionToExpr @@ var "inc"])],
      statementToExpr @@ var "body"]

functionApplicationToExpr :: TTermDefinition (Cpp.FunctionApplication -> Expr)
functionApplicationToExpr = define "functionApplicationToExpr" $
  doc "Convert a function application to an expression" $
  lambda "fa" $ lets [
    "func">: project Cpp._FunctionApplication Cpp._FunctionApplication_function @@ var "fa",
    "args">: project Cpp._FunctionApplication Cpp._FunctionApplication_arguments @@ var "fa"] $
    Serialization.spaceSep @@ list [
      functionIdentifierToExpr @@ var "func",
      Serialization.parenListAdaptive @@ (Lists.map expressionToExpr (var "args"))]

functionBodyToExpr :: TTermDefinition (Cpp.FunctionBody -> Expr)
functionBodyToExpr = define "functionBodyToExpr" $
  doc "Convert a function body to an expression" $
  lambda "b" $
    cases Cpp._FunctionBody (var "b") Nothing [
      Cpp._FunctionBody_compound>>: lambda "c" $ compoundStatementToExpr @@ var "c",
      Cpp._FunctionBody_declaration>>: constant $ Serialization.cst @@ string ";",
      Cpp._FunctionBody_pure>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "= 0"),
      Cpp._FunctionBody_default>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "= default")]

functionCallOperationToExpr :: TTermDefinition (Cpp.FunctionCallOperation -> Expr)
functionCallOperationToExpr = define "functionCallOperationToExpr" $
  doc "Convert a function call operation to an expression" $
  lambda "fco" $ lets [
    "func">: project Cpp._FunctionCallOperation Cpp._FunctionCallOperation_function @@ var "fco",
    "args">: project Cpp._FunctionCallOperation Cpp._FunctionCallOperation_arguments @@ var "fco"] $
    Serialization.noSep @@ list [
      postfixExpressionToExpr @@ var "func",
      Serialization.parenListAdaptive @@ (Lists.map expressionToExpr (var "args"))]

functionDeclarationToExpr :: TTermDefinition (Cpp.FunctionDeclaration -> Expr)
functionDeclarationToExpr = define "functionDeclarationToExpr" $
  doc "Convert a function declaration to an expression" $
  lambda "fd" $ lets [
    "prefixSpecs">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_prefixSpecifiers @@ var "fd",
    "retType">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_returnType @@ var "fd",
    "name">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_name @@ var "fd",
    "params">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_parameters @@ var "fd",
    "suffixSpecs">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_suffixSpecifiers @@ var "fd",
    "body">: project Cpp._FunctionDeclaration Cpp._FunctionDeclaration_body @@ var "fd"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      Lists.map functionSpecifierPrefixToExpr (var "prefixSpecs"),
      list [
        typeExpressionToExpr @@ var "retType",
        Serialization.noSep @@ list [
          Serialization.cst @@ var "name",
          Serialization.parenListAdaptive @@ (Lists.map parameterToExpr (var "params"))]],
      Lists.map functionSpecifierSuffixToExpr (var "suffixSpecs"),
      list [functionBodyToExpr @@ var "body"]])

functionIdentifierToExpr :: TTermDefinition (Cpp.FunctionIdentifier -> Expr)
functionIdentifierToExpr = define "functionIdentifierToExpr" $
  doc "Convert a function identifier to an expression" $
  lambda "f" $
    cases Cpp._FunctionIdentifier (var "f") Nothing [
      Cpp._FunctionIdentifier_simple>>: lambda "name" $ Serialization.cst @@ var "name",
      Cpp._FunctionIdentifier_qualified>>: lambda "q" $ qualifiedIdentifierToExpr @@ var "q"]

functionSpecifierPrefixToExpr :: TTermDefinition (Cpp.FunctionSpecifierPrefix -> Expr)
functionSpecifierPrefixToExpr = define "functionSpecifierPrefixToExpr" $
  doc "Convert a function specifier prefix to an expression" $
  lambda "s" $
    cases Cpp._FunctionSpecifierPrefix (var "s") Nothing [
      Cpp._FunctionSpecifierPrefix_inline>>: constant $ Serialization.cst @@ string "inline",
      Cpp._FunctionSpecifierPrefix_virtual>>: constant $ Serialization.cst @@ string "virtual",
      Cpp._FunctionSpecifierPrefix_static>>: constant $ Serialization.cst @@ string "static",
      Cpp._FunctionSpecifierPrefix_explicit>>: constant $ Serialization.cst @@ string "explicit"]

functionSpecifierSuffixToExpr :: TTermDefinition (Cpp.FunctionSpecifierSuffix -> Expr)
functionSpecifierSuffixToExpr = define "functionSpecifierSuffixToExpr" $
  doc "Convert a function specifier suffix to an expression" $
  lambda "s" $
    cases Cpp._FunctionSpecifierSuffix (var "s") Nothing [
      Cpp._FunctionSpecifierSuffix_const>>: constant $ Serialization.cst @@ string "const",
      Cpp._FunctionSpecifierSuffix_noexcept>>: constant $ Serialization.cst @@ string "noexcept",
      Cpp._FunctionSpecifierSuffix_override>>: constant $ Serialization.cst @@ string "override",
      Cpp._FunctionSpecifierSuffix_final>>: constant $ Serialization.cst @@ string "final"]

functionTypeToExpr :: TTermDefinition (Cpp.FunctionType -> Expr)
functionTypeToExpr = define "functionTypeToExpr" $
  doc "Convert a function type to an expression" $
  lambda "ft" $ lets [
    "retType">: project Cpp._FunctionType Cpp._FunctionType_returnType @@ var "ft",
    "params">: project Cpp._FunctionType Cpp._FunctionType_parameters @@ var "ft"] $
    Serialization.spaceSep @@ list [
      typeExpressionToExpr @@ var "retType",
      Serialization.parenListAdaptive @@ (Lists.map parameterToExpr (var "params"))]

greaterEqualOperationToExpr :: TTermDefinition (Cpp.GreaterEqualOperation -> Expr)
greaterEqualOperationToExpr = define "greaterEqualOperationToExpr" $
  doc "Convert a greater-than-or-equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._GreaterEqualOperation Cpp._GreaterEqualOperation_left @@ var "op",
    "right">: project Cpp._GreaterEqualOperation Cpp._GreaterEqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      relationalExpressionToExpr @@ var "left",
      Serialization.cst @@ string ">=",
      shiftExpressionToExpr @@ var "right"]

greaterOperationToExpr :: TTermDefinition (Cpp.GreaterOperation -> Expr)
greaterOperationToExpr = define "greaterOperationToExpr" $
  doc "Convert a greater-than operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._GreaterOperation Cpp._GreaterOperation_left @@ var "op",
    "right">: project Cpp._GreaterOperation Cpp._GreaterOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      relationalExpressionToExpr @@ var "left",
      Serialization.cst @@ string ">",
      shiftExpressionToExpr @@ var "right"]

ifDirectiveToExpr :: TTermDefinition (Cpp.IfDirective -> Expr)
ifDirectiveToExpr = define "ifDirectiveToExpr" $
  doc "Convert an if directive to an expression" $
  lambda "ifd" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#if",
      Serialization.cst @@ (project Cpp._IfDirective Cpp._IfDirective_condition @@ var "ifd")]

ifdefDirectiveToExpr :: TTermDefinition (Cpp.IfdefDirective -> Expr)
ifdefDirectiveToExpr = define "ifdefDirectiveToExpr" $
  doc "Convert an ifdef directive to an expression" $
  lambda "id" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#ifdef",
      Serialization.cst @@ (project Cpp._IfdefDirective Cpp._IfdefDirective_identifier @@ var "id")]

ifndefDirectiveToExpr :: TTermDefinition (Cpp.IfndefDirective -> Expr)
ifndefDirectiveToExpr = define "ifndefDirectiveToExpr" $
  doc "Convert an ifndef directive to an expression" $
  lambda "ind" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#ifndef",
      Serialization.cst @@ (project Cpp._IfndefDirective Cpp._IfndefDirective_identifier @@ var "ind")]

includeDirectiveToExpr :: TTermDefinition (Cpp.IncludeDirective -> Expr)
includeDirectiveToExpr = define "includeDirectiveToExpr" $
  doc "Convert an include directive to an expression" $
  lambda "incl" $ lets [
    "name">: project Cpp._IncludeDirective Cpp._IncludeDirective_name @@ var "incl",
    "isSystem">: project Cpp._IncludeDirective Cpp._IncludeDirective_isSystem @@ var "incl"] $
    Logic.ifElse (var "isSystem")
      (Serialization.cst @@ (Strings.cat $ list [string "#include <", var "name", string ">"]))
      (Serialization.cst @@ (Strings.cat $ list [string "#include \"", var "name", string "\""]))

inclusiveOrExpressionToExpr :: TTermDefinition (Cpp.InclusiveOrExpression -> Expr)
inclusiveOrExpressionToExpr = define "inclusiveOrExpressionToExpr" $
  doc "Convert an inclusive or expression to an expression" $
  lambda "e" $
    cases Cpp._InclusiveOrExpression (var "e") Nothing [
      Cpp._InclusiveOrExpression_exclusiveOr>>: lambda "x" $ exclusiveOrExpressionToExpr @@ var "x",
      Cpp._InclusiveOrExpression_bitwiseOr>>: lambda "o" $ bitwiseOrOperationToExpr @@ var "o"]

integerLiteralToExpr :: TTermDefinition (Cpp.IntegerLiteral -> Expr)
integerLiteralToExpr = define "integerLiteralToExpr" $
  doc "Convert an integer literal to an expression" $
  lambda "i" $
    cases Cpp._IntegerLiteral (var "i") Nothing [
      Cpp._IntegerLiteral_decimal>>: lambda "n" $ Serialization.cst @@ (Literals.showBigint (var "n")),
      Cpp._IntegerLiteral_hexadecimal>>: lambda "h" $ Serialization.cst @@ (Strings.cat2 (string "0x") (var "h")),
      Cpp._IntegerLiteral_octal>>: lambda "o" $ Serialization.cst @@ (Strings.cat2 (string "0") (var "o")),
      Cpp._IntegerLiteral_binary>>: lambda "b" $ Serialization.cst @@ (Strings.cat2 (string "0b") (var "b"))]

iterationStatementToExpr :: TTermDefinition (Cpp.IterationStatement -> Expr)
iterationStatementToExpr = define "iterationStatementToExpr" $
  doc "Convert an iteration statement to an expression" $
  lambda "i" $
    cases Cpp._IterationStatement (var "i") Nothing [
      Cpp._IterationStatement_while>>: lambda "w" $ whileStatementToExpr @@ var "w",
      Cpp._IterationStatement_do>>: lambda "d" $ doStatementToExpr @@ var "d",
      Cpp._IterationStatement_for>>: lambda "f" $ forStatementToExpr @@ var "f",
      Cpp._IterationStatement_rangeFor>>: lambda "r" $ rangeForStatementToExpr @@ var "r"]

jumpStatementToExpr :: TTermDefinition (Cpp.JumpStatement -> Expr)
jumpStatementToExpr = define "jumpStatementToExpr" $
  doc "Convert a jump statement to an expression" $
  lambda "j" $
    cases Cpp._JumpStatement (var "j") Nothing [
      Cpp._JumpStatement_break>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "break"),
      Cpp._JumpStatement_continue>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "continue"),
      Cpp._JumpStatement_returnValue>>: lambda "e" $
        Serialization.withSemi @@ (Serialization.spaceSep @@ list [Serialization.cst @@ string "return", expressionToExpr @@ var "e"]),
      Cpp._JumpStatement_returnVoid>>: constant $ Serialization.withSemi @@ (Serialization.cst @@ string "return"),
      Cpp._JumpStatement_throw>>: lambda "e" $
        Serialization.withSemi @@ (Serialization.spaceSep @@ list [Serialization.cst @@ string "throw", expressionToExpr @@ var "e"])]

labeledStatementToExpr :: TTermDefinition (Cpp.LabeledStatement -> Expr)
labeledStatementToExpr = define "labeledStatementToExpr" $
  doc "Convert a labeled statement to an expression" $
  lambda "ls" $ lets [
    "label">: project Cpp._LabeledStatement Cpp._LabeledStatement_label @@ var "ls",
    "stmt">: project Cpp._LabeledStatement Cpp._LabeledStatement_statement @@ var "ls"] $
    Serialization.newlineSep @@ list [
      Serialization.cst @@ (Strings.cat2 (var "label") (string ":")),
      statementToExpr @@ var "stmt"]

lambdaExpressionToExpr :: TTermDefinition (Cpp.LambdaExpression -> Expr)
lambdaExpressionToExpr = define "lambdaExpressionToExpr" $
  doc "Convert a lambda expression to an expression" $
  lambda "le" $ lets [
    "captures">: project Cpp._LambdaExpression Cpp._LambdaExpression_captures @@ var "le",
    "params">: project Cpp._LambdaExpression Cpp._LambdaExpression_parameters @@ var "le",
    "retType">: project Cpp._LambdaExpression Cpp._LambdaExpression_returnType @@ var "le",
    "body">: project Cpp._LambdaExpression Cpp._LambdaExpression_body @@ var "le"] $
    Serialization.spaceSep @@ list [
      captureListToExpr @@ var "captures",
      Logic.ifElse (Lists.null (var "params"))
        (Serialization.parens @@ (Serialization.cst @@ string ""))
        (Serialization.parenListAdaptive @@ (Lists.map parameterToExpr (var "params"))),
      Maybes.maybe
        (Serialization.cst @@ string "")
        (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "->", typeExpressionToExpr @@ var "t"])
        (var "retType"),
      compoundStatementToExpr @@ var "body"]

leftShiftOperationToExpr :: TTermDefinition (Cpp.LeftShiftOperation -> Expr)
leftShiftOperationToExpr = define "leftShiftOperationToExpr" $
  doc "Convert a left shift operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LeftShiftOperation Cpp._LeftShiftOperation_left @@ var "op",
    "right">: project Cpp._LeftShiftOperation Cpp._LeftShiftOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      shiftExpressionToExpr @@ var "left",
      Serialization.cst @@ string "<<",
      additiveExpressionToExpr @@ var "right"]

lessEqualOperationToExpr :: TTermDefinition (Cpp.LessEqualOperation -> Expr)
lessEqualOperationToExpr = define "lessEqualOperationToExpr" $
  doc "Convert a less-than-or-equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LessEqualOperation Cpp._LessEqualOperation_left @@ var "op",
    "right">: project Cpp._LessEqualOperation Cpp._LessEqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      relationalExpressionToExpr @@ var "left",
      Serialization.cst @@ string "<=",
      shiftExpressionToExpr @@ var "right"]

lessOperationToExpr :: TTermDefinition (Cpp.LessOperation -> Expr)
lessOperationToExpr = define "lessOperationToExpr" $
  doc "Convert a less-than operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LessOperation Cpp._LessOperation_left @@ var "op",
    "right">: project Cpp._LessOperation Cpp._LessOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      relationalExpressionToExpr @@ var "left",
      Serialization.cst @@ string "<",
      shiftExpressionToExpr @@ var "right"]

lineDirectiveToExpr :: TTermDefinition (Cpp.LineDirective -> Expr)
lineDirectiveToExpr = define "lineDirectiveToExpr" $
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

literalToExpr :: TTermDefinition (Cpp.Literal -> Expr)
literalToExpr = define "literalToExpr" $
  doc "Convert a literal to an expression" $
  lambda "l" $
    cases Cpp._Literal (var "l") Nothing [
      Cpp._Literal_integer>>: lambda "i" $ integerLiteralToExpr @@ var "i",
      Cpp._Literal_floating>>: lambda "f" $ Serialization.cst @@ (Literals.showBigfloat (unwrap Cpp._FloatingLiteral @@ var "f")),
      Cpp._Literal_character>>: lambda "c" $
        Serialization.cst @@ (Strings.cat $ list [string "'", unwrap Cpp._CharacterLiteral @@ var "c", string "'"]),
      Cpp._Literal_string>>: lambda "s" $
        Serialization.cst @@ (Strings.cat $ list [string "\"", unwrap Cpp._StringLiteral @@ var "s", string "\""]),
      Cpp._Literal_boolean>>: lambda "b" $ booleanLiteralToExpr @@ var "b",
      Cpp._Literal_null>>: constant $ Serialization.cst @@ string "nullptr"]

logicalAndExpressionToExpr :: TTermDefinition (Cpp.LogicalAndExpression -> Expr)
logicalAndExpressionToExpr = define "logicalAndExpressionToExpr" $
  doc "Convert a logical and expression to an expression" $
  lambda "e" $
    cases Cpp._LogicalAndExpression (var "e") Nothing [
      Cpp._LogicalAndExpression_inclusiveOr>>: lambda "i" $ inclusiveOrExpressionToExpr @@ var "i",
      Cpp._LogicalAndExpression_logicalAnd>>: lambda "a" $ logicalAndOperationToExpr @@ var "a"]

logicalAndOperationToExpr :: TTermDefinition (Cpp.LogicalAndOperation -> Expr)
logicalAndOperationToExpr = define "logicalAndOperationToExpr" $
  doc "Convert a logical and operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LogicalAndOperation Cpp._LogicalAndOperation_left @@ var "op",
    "right">: project Cpp._LogicalAndOperation Cpp._LogicalAndOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      logicalAndExpressionToExpr @@ var "left",
      Serialization.cst @@ string "&&",
      inclusiveOrExpressionToExpr @@ var "right"]

logicalOrExpressionToExpr :: TTermDefinition (Cpp.LogicalOrExpression -> Expr)
logicalOrExpressionToExpr = define "logicalOrExpressionToExpr" $
  doc "Convert a logical or expression to an expression" $
  lambda "e" $
    cases Cpp._LogicalOrExpression (var "e") Nothing [
      Cpp._LogicalOrExpression_logicalAnd>>: lambda "l" $ logicalAndExpressionToExpr @@ var "l",
      Cpp._LogicalOrExpression_logicalOr>>: lambda "o" $ logicalOrOperationToExpr @@ var "o"]

logicalOrOperationToExpr :: TTermDefinition (Cpp.LogicalOrOperation -> Expr)
logicalOrOperationToExpr = define "logicalOrOperationToExpr" $
  doc "Convert a logical or operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._LogicalOrOperation Cpp._LogicalOrOperation_left @@ var "op",
    "right">: project Cpp._LogicalOrOperation Cpp._LogicalOrOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      logicalOrExpressionToExpr @@ var "left",
      Serialization.cst @@ string "||",
      logicalAndExpressionToExpr @@ var "right"]

mapToExpr :: TTermDefinition (Cpp.Map -> Expr)
mapToExpr = define "mapToExpr" $
  doc "Convert a map to an expression" $
  lambda "m" $ lets [
    "keyType">: project Cpp._Map Cpp._Map_keyType @@ var "m",
    "valType">: project Cpp._Map Cpp._Map_valueType @@ var "m",
    "entries">: project Cpp._Map Cpp._Map_entries @@ var "m"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::map<",
      Serialization.commaSep @@ Serialization.inlineStyle @@ list [
        typeExpressionToExpr @@ var "keyType",
        typeExpressionToExpr @@ var "valType"],
      Serialization.cst @@ string ">",
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ (Lists.map mapEntryToExpr (var "entries"))]

mapEntryToExpr :: TTermDefinition (Cpp.MapEntry -> Expr)
mapEntryToExpr = define "mapEntryToExpr" $
  doc "Convert a map entry to an expression" $
  lambda "me" $ lets [
    "key">: project Cpp._MapEntry Cpp._MapEntry_key @@ var "me",
    "val">: project Cpp._MapEntry Cpp._MapEntry_value @@ var "me"] $
    Serialization.spaceSep @@ list [
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ list [expressionToExpr @@ var "key"],
      Serialization.cst @@ string "->",
      expressionToExpr @@ var "val"]

memInitializerToExpr :: TTermDefinition (Cpp.MemInitializer -> Expr)
memInitializerToExpr = define "memInitializerToExpr" $
  doc "Convert a member initializer to an expression" $
  lambda "mi" $ lets [
    "name">: project Cpp._MemInitializer Cpp._MemInitializer_name @@ var "mi",
    "args">: project Cpp._MemInitializer Cpp._MemInitializer_arguments @@ var "mi"] $
    Serialization.noSep @@ list [
      Serialization.cst @@ var "name",
      Serialization.parenListAdaptive @@ (Lists.map expressionToExpr (var "args"))]

memberAccessOperationToExpr :: TTermDefinition (Cpp.MemberAccessOperation -> Expr)
memberAccessOperationToExpr = define "memberAccessOperationToExpr" $
  doc "Convert a member access operation to an expression" $
  lambda "mao" $ lets [
    "obj">: project Cpp._MemberAccessOperation Cpp._MemberAccessOperation_object @@ var "mao",
    "member">: project Cpp._MemberAccessOperation Cpp._MemberAccessOperation_member @@ var "mao"] $
    Serialization.noSep @@ list [
      postfixExpressionToExpr @@ var "obj",
      Serialization.cst @@ string ".",
      Serialization.cst @@ var "member"]

memberDeclarationToExpr :: TTermDefinition (Bool -> Cpp.MemberDeclaration -> Expr)
memberDeclarationToExpr = define "memberDeclarationToExpr" $
  doc "Convert a member declaration to an expression" $
  lambda "commas" $ lambda "m" $
    cases Cpp._MemberDeclaration (var "m") Nothing [
      Cpp._MemberDeclaration_function>>: lambda "f" $ functionDeclarationToExpr @@ var "f",
      Cpp._MemberDeclaration_variable>>: lambda "v" $ variableDeclarationToExpr @@ var "commas" @@ var "v",
      Cpp._MemberDeclaration_constructor>>: lambda "c" $ constructorDeclarationToExpr @@ var "c",
      Cpp._MemberDeclaration_destructor>>: lambda "d" $ destructorDeclarationToExpr @@ var "d",
      Cpp._MemberDeclaration_nestedClass>>: lambda "c" $ classDeclarationToExpr @@ var "c",
      Cpp._MemberDeclaration_template>>: lambda "t" $ templateDeclarationToExpr @@ var "t"]

memberSpecificationToExpr :: TTermDefinition (Bool -> Cpp.MemberSpecification -> Expr)
memberSpecificationToExpr = define "memberSpecificationToExpr" $
  doc "Convert a member specification to an expression" $
  lambda "commas" $ lambda "m" $
    cases Cpp._MemberSpecification (var "m") Nothing [
      Cpp._MemberSpecification_accessLabel>>: lambda "a" $
        Serialization.noSep @@ list [accessSpecifierToExpr @@ var "a", Serialization.cst @@ string ":"],
      Cpp._MemberSpecification_member>>: lambda "d" $ memberDeclarationToExpr @@ var "commas" @@ var "d"]

moduloOperationToExpr :: TTermDefinition (Cpp.ModuloOperation -> Expr)
moduloOperationToExpr = define "moduloOperationToExpr" $
  doc "Convert a modulo operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._ModuloOperation Cpp._ModuloOperation_left @@ var "op",
    "right">: project Cpp._ModuloOperation Cpp._ModuloOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      multiplicativeExpressionToExpr @@ var "left",
      Serialization.cst @@ string "%",
      unaryExpressionToExpr @@ var "right"]

multiplicativeExpressionToExpr :: TTermDefinition (Cpp.MultiplicativeExpression -> Expr)
multiplicativeExpressionToExpr = define "multiplicativeExpressionToExpr" $
  doc "Convert a multiplicative expression to an expression" $
  lambda "e" $
    cases Cpp._MultiplicativeExpression (var "e") Nothing [
      Cpp._MultiplicativeExpression_unary>>: lambda "u" $ unaryExpressionToExpr @@ var "u",
      Cpp._MultiplicativeExpression_multiply>>: lambda "m" $ multiplyOperationToExpr @@ var "m",
      Cpp._MultiplicativeExpression_divide>>: lambda "d" $ divideOperationToExpr @@ var "d",
      Cpp._MultiplicativeExpression_modulo>>: lambda "m" $ moduloOperationToExpr @@ var "m"]

multiplyOperationToExpr :: TTermDefinition (Cpp.MultiplyOperation -> Expr)
multiplyOperationToExpr = define "multiplyOperationToExpr" $
  doc "Convert a multiply operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._MultiplyOperation Cpp._MultiplyOperation_left @@ var "op",
    "right">: project Cpp._MultiplyOperation Cpp._MultiplyOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      multiplicativeExpressionToExpr @@ var "left",
      Serialization.cst @@ string "*",
      unaryExpressionToExpr @@ var "right"]

namespaceDeclarationToExpr :: TTermDefinition (Cpp.NamespaceDeclaration -> Expr)
namespaceDeclarationToExpr = define "namespaceDeclarationToExpr" $
  doc "Convert a namespace declaration to an expression" $
  lambda "nd" $ lets [
    "name">: project Cpp._NamespaceDeclaration Cpp._NamespaceDeclaration_name @@ var "nd",
    "decls">: project Cpp._NamespaceDeclaration Cpp._NamespaceDeclaration_declarations @@ var "nd"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ (Strings.cat2 (string "namespace ") (var "name")),
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
        (Serialization.doubleNewlineSep @@ (Lists.map declarationToExpr (var "decls")))]

notEqualOperationToExpr :: TTermDefinition (Cpp.NotEqualOperation -> Expr)
notEqualOperationToExpr = define "notEqualOperationToExpr" $
  doc "Convert a not-equal operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._NotEqualOperation Cpp._NotEqualOperation_left @@ var "op",
    "right">: project Cpp._NotEqualOperation Cpp._NotEqualOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      equalityExpressionToExpr @@ var "left",
      Serialization.cst @@ string "!=",
      relationalExpressionToExpr @@ var "right"]

optionalToExpr :: TTermDefinition (Cpp.Optional -> Expr)
optionalToExpr = define "optionalToExpr" $
  doc "Convert an optional to an expression" $
  lambda "opt" $ lets [
    "valType">: project Cpp._Optional Cpp._Optional_valueType @@ var "opt",
    "val">: project Cpp._Optional Cpp._Optional_value @@ var "opt"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::optional<",
      typeExpressionToExpr @@ var "valType",
      Serialization.cst @@ string ">",
      Maybes.maybe
        (Serialization.cst @@ string "{}")
        (lambda "v" $ Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ list [expressionToExpr @@ var "v"])
        (var "val")]

overloadedLambdasToExpr :: TTermDefinition (Cpp.OverloadedLambdas -> Expr)
overloadedLambdasToExpr = define "overloadedLambdasToExpr" $
  doc "Convert overloaded lambdas to an expression" $
  lambda "ol" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "overloaded",
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
        (Serialization.newlineSep @@ (Lists.map lambdaExpressionToExpr (unwrap Cpp._OverloadedLambdas @@ var "ol")))]

parameterToExpr :: TTermDefinition (Cpp.Parameter -> Expr)
parameterToExpr = define "parameterToExpr" $
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
      list [typeExpressionToExpr @@ var "typ", var "nameExpr"],
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "expr" $ list [Serialization.cst @@ string "=", expressionToExpr @@ var "expr"])
        (var "defaultVal")])

patternMatchToExpr :: TTermDefinition (Cpp.PatternMatch -> Expr)
patternMatchToExpr = define "patternMatchToExpr" $
  doc "Convert a pattern match to an expression" $
  lambda "pm" $ lets [
    "visitor">: project Cpp._PatternMatch Cpp._PatternMatch_visitor @@ var "pm",
    "variant">: project Cpp._PatternMatch Cpp._PatternMatch_variant @@ var "pm"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::visit",
      Serialization.parenListAdaptive @@ list [
        visitorToExpr @@ var "visitor",
        expressionToExpr @@ var "variant"]]

pointerMemberAccessOperationToExpr :: TTermDefinition (Cpp.PointerMemberAccessOperation -> Expr)
pointerMemberAccessOperationToExpr = define "pointerMemberAccessOperationToExpr" $
  doc "Convert a pointer member access operation to an expression" $
  lambda "pmao" $ lets [
    "ptr">: project Cpp._PointerMemberAccessOperation Cpp._PointerMemberAccessOperation_pointer @@ var "pmao",
    "member">: project Cpp._PointerMemberAccessOperation Cpp._PointerMemberAccessOperation_member @@ var "pmao"] $
    Serialization.noSep @@ list [
      postfixExpressionToExpr @@ var "ptr",
      Serialization.cst @@ string "->",
      Serialization.cst @@ var "member"]

postfixExpressionToExpr :: TTermDefinition (Cpp.PostfixExpression -> Expr)
postfixExpressionToExpr = define "postfixExpressionToExpr" $
  doc "Convert a postfix expression to an expression" $
  lambda "e" $
    cases Cpp._PostfixExpression (var "e") Nothing [
      Cpp._PostfixExpression_primary>>: lambda "p" $ primaryExpressionToExpr @@ var "p",
      Cpp._PostfixExpression_subscript>>: lambda "s" $ subscriptOperationToExpr @@ var "s",
      Cpp._PostfixExpression_functionCall>>: lambda "f" $ functionCallOperationToExpr @@ var "f",
      Cpp._PostfixExpression_templateFunctionCall>>: lambda "t" $ templateFunctionCallOperationToExpr @@ var "t",
      Cpp._PostfixExpression_memberAccess>>: lambda "m" $ memberAccessOperationToExpr @@ var "m",
      Cpp._PostfixExpression_pointerMemberAccess>>: lambda "p" $ pointerMemberAccessOperationToExpr @@ var "p",
      Cpp._PostfixExpression_postIncrement>>: lambda "p" $
        Serialization.noSep @@ list [postfixExpressionToExpr @@ var "p", Serialization.cst @@ string "++"],
      Cpp._PostfixExpression_postDecrement>>: lambda "p" $
        Serialization.noSep @@ list [postfixExpressionToExpr @@ var "p", Serialization.cst @@ string "--"]]

pragmaDirectiveToExpr :: TTermDefinition (Cpp.PragmaDirective -> Expr)
pragmaDirectiveToExpr = define "pragmaDirectiveToExpr" $
  doc "Convert a pragma directive to an expression" $
  lambda "pd" $
    Serialization.cst @@ (Strings.cat2 (string "#pragma ") (project Cpp._PragmaDirective Cpp._PragmaDirective_content @@ var "pd"))

preprocessorDirectiveToExpr :: TTermDefinition (Cpp.PreprocessorDirective -> Expr)
preprocessorDirectiveToExpr = define "preprocessorDirectiveToExpr" $
  doc "Convert a preprocessor directive to an expression" $
  lambda "d" $
    cases Cpp._PreprocessorDirective (var "d") Nothing [
      Cpp._PreprocessorDirective_include>>: lambda "i" $ includeDirectiveToExpr @@ var "i",
      Cpp._PreprocessorDirective_pragma>>: lambda "p" $ pragmaDirectiveToExpr @@ var "p",
      Cpp._PreprocessorDirective_define>>: lambda "d" $ defineDirectiveToExpr @@ var "d",
      Cpp._PreprocessorDirective_undef>>: lambda "u" $ undefDirectiveToExpr @@ var "u",
      Cpp._PreprocessorDirective_ifdef>>: lambda "i" $ ifdefDirectiveToExpr @@ var "i",
      Cpp._PreprocessorDirective_ifndef>>: lambda "i" $ ifndefDirectiveToExpr @@ var "i",
      Cpp._PreprocessorDirective_if>>: lambda "i" $ ifDirectiveToExpr @@ var "i",
      Cpp._PreprocessorDirective_elif>>: lambda "e" $ elifDirectiveToExpr @@ var "e",
      Cpp._PreprocessorDirective_else>>: lambda "e" $ elseDirectiveToExpr @@ var "e",
      Cpp._PreprocessorDirective_endif>>: lambda "e" $ endifDirectiveToExpr @@ var "e",
      Cpp._PreprocessorDirective_line>>: lambda "l" $ lineDirectiveToExpr @@ var "l",
      Cpp._PreprocessorDirective_error>>: lambda "e" $ errorDirectiveToExpr @@ var "e",
      Cpp._PreprocessorDirective_warning>>: lambda "w" $ warningDirectiveToExpr @@ var "w"]

primaryExpressionToExpr :: TTermDefinition (Cpp.PrimaryExpression -> Expr)
primaryExpressionToExpr = define "primaryExpressionToExpr" $
  doc "Convert a primary expression to an expression" $
  lambda "e" $
    cases Cpp._PrimaryExpression (var "e") Nothing [
      Cpp._PrimaryExpression_identifier>>: lambda "id" $ Serialization.cst @@ var "id",
      Cpp._PrimaryExpression_literal>>: lambda "l" $ literalToExpr @@ var "l",
      Cpp._PrimaryExpression_parenthesized>>: lambda "p" $ Serialization.parens @@ (expressionToExpr @@ var "p"),
      Cpp._PrimaryExpression_lambda>>: lambda "l" $ lambdaExpressionToExpr @@ var "l"]

programToExpr :: TTermDefinition (Cpp.Program -> Expr)
programToExpr = define "programToExpr" $
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
      var "separate" @@ Serialization.newlineSep @@ (Lists.map preprocessorDirectiveToExpr (var "preps")),
      var "separate" @@ Serialization.newlineSep @@ (Lists.map includeDirectiveToExpr (var "includes")),
      var "separate" @@ Serialization.doubleNewlineSep @@ (Lists.map declarationToExpr (var "decls"))])

qualifiedIdentifierToExpr :: TTermDefinition (Cpp.QualifiedIdentifier -> Expr)
qualifiedIdentifierToExpr = define "qualifiedIdentifierToExpr" $
  doc "Convert a qualified identifier to an expression" $
  lambda "qi" $ lets [
    "ns">: project Cpp._QualifiedIdentifier Cpp._QualifiedIdentifier_namespace @@ var "qi",
    "name">: project Cpp._QualifiedIdentifier Cpp._QualifiedIdentifier_name @@ var "qi"] $
    Serialization.cst @@ (Strings.cat $ list [var "ns", string "::", var "name"])

qualifiedTypeToExpr :: TTermDefinition (Cpp.QualifiedType -> Expr)
qualifiedTypeToExpr = define "qualifiedTypeToExpr" $
  doc "Convert a qualified type to an expression" $
  lambda "qt" $ lets [
    "baseType">: project Cpp._QualifiedType Cpp._QualifiedType_baseType @@ var "qt",
    "qualifier">: project Cpp._QualifiedType Cpp._QualifiedType_qualifier @@ var "qt"] $
    cases Cpp._TypeQualifier (var "qualifier") Nothing [
      Cpp._TypeQualifier_const>>: constant $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "const", typeExpressionToExpr @@ var "baseType"],
      Cpp._TypeQualifier_lvalueRef>>: constant $
        Serialization.noSep @@ list [typeExpressionToExpr @@ var "baseType", Serialization.cst @@ string "&"],
      Cpp._TypeQualifier_rvalueRef>>: constant $
        Serialization.noSep @@ list [typeExpressionToExpr @@ var "baseType", Serialization.cst @@ string "&&"],
      Cpp._TypeQualifier_pointer>>: constant $
        Serialization.noSep @@ list [typeExpressionToExpr @@ var "baseType", Serialization.cst @@ string "*"]]

rangeForStatementToExpr :: TTermDefinition (Cpp.RangeForStatement -> Expr)
rangeForStatementToExpr = define "rangeForStatementToExpr" $
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
          typeExpressionToExpr @@ var "typ",
          Serialization.cst @@ var "var",
          Serialization.cst @@ string ":",
          expressionToExpr @@ var "range"])],
      statementToExpr @@ var "body"]

relationalExpressionToExpr :: TTermDefinition (Cpp.RelationalExpression -> Expr)
relationalExpressionToExpr = define "relationalExpressionToExpr" $
  doc "Convert a relational expression to an expression" $
  lambda "e" $
    cases Cpp._RelationalExpression (var "e") Nothing [
      Cpp._RelationalExpression_shift>>: lambda "s" $ shiftExpressionToExpr @@ var "s",
      Cpp._RelationalExpression_less>>: lambda "l" $ lessOperationToExpr @@ var "l",
      Cpp._RelationalExpression_greater>>: lambda "g" $ greaterOperationToExpr @@ var "g",
      Cpp._RelationalExpression_lessEqual>>: lambda "le" $ lessEqualOperationToExpr @@ var "le",
      Cpp._RelationalExpression_greaterEqual>>: lambda "ge" $ greaterEqualOperationToExpr @@ var "ge"]

rightShiftOperationToExpr :: TTermDefinition (Cpp.RightShiftOperation -> Expr)
rightShiftOperationToExpr = define "rightShiftOperationToExpr" $
  doc "Convert a right shift operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._RightShiftOperation Cpp._RightShiftOperation_left @@ var "op",
    "right">: project Cpp._RightShiftOperation Cpp._RightShiftOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      shiftExpressionToExpr @@ var "left",
      Serialization.cst @@ string ">>",
      additiveExpressionToExpr @@ var "right"]

selectionStatementToExpr :: TTermDefinition (Cpp.SelectionStatement -> Expr)
selectionStatementToExpr = define "selectionStatementToExpr" $
  doc "Convert a selection statement to an expression" $
  lambda "ss" $ lets [
    "cond">: project Cpp._SelectionStatement Cpp._SelectionStatement_condition @@ var "ss",
    "thenBranch">: project Cpp._SelectionStatement Cpp._SelectionStatement_thenBranch @@ var "ss",
    "elseBranch">: project Cpp._SelectionStatement Cpp._SelectionStatement_elseBranch @@ var "ss"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "if",
        Serialization.parens @@ (expressionToExpr @@ var "cond")],
      statementToExpr @@ var "thenBranch",
      Maybes.maybe
        (Serialization.cst @@ string "")
        (lambda "stmt" $ Serialization.newlineSep @@ list [Serialization.cst @@ string "else", statementToExpr @@ var "stmt"])
        (var "elseBranch")]

setToExpr :: TTermDefinition (Cpp.Set -> Expr)
setToExpr = define "setToExpr" $
  doc "Convert a set to an expression" $
  lambda "s" $ lets [
    "elemType">: project Cpp._Set Cpp._Set_elementType @@ var "s",
    "elems">: project Cpp._Set Cpp._Set_elements @@ var "s"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::set<",
      typeExpressionToExpr @@ var "elemType",
      Serialization.cst @@ string ">",
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ (Lists.map expressionToExpr (var "elems"))]

shiftExpressionToExpr :: TTermDefinition (Cpp.ShiftExpression -> Expr)
shiftExpressionToExpr = define "shiftExpressionToExpr" $
  doc "Convert a shift expression to an expression" $
  lambda "e" $
    cases Cpp._ShiftExpression (var "e") Nothing [
      Cpp._ShiftExpression_additive>>: lambda "a" $ additiveExpressionToExpr @@ var "a",
      Cpp._ShiftExpression_leftShift>>: lambda "ls" $ leftShiftOperationToExpr @@ var "ls",
      Cpp._ShiftExpression_rightShift>>: lambda "rs" $ rightShiftOperationToExpr @@ var "rs"]

sizeofExpressionToExpr :: TTermDefinition (Cpp.SizeofExpression -> Expr)
sizeofExpressionToExpr = define "sizeofExpressionToExpr" $
  doc "Convert a sizeof expression to an expression" $
  lambda "se" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "sizeof",
      Serialization.parens @@ (typeExpressionToExpr @@ (unwrap Cpp._SizeofExpression @@ var "se"))]

statementToExpr :: TTermDefinition (Cpp.Statement -> Expr)
statementToExpr = define "statementToExpr" $
  doc "Convert a statement to an expression" $
  lambda "s" $
    cases Cpp._Statement (var "s") Nothing [
      Cpp._Statement_labeled>>: lambda "l" $ labeledStatementToExpr @@ var "l",
      Cpp._Statement_compound>>: lambda "c" $ compoundStatementToExpr @@ var "c",
      Cpp._Statement_selection>>: lambda "s" $ selectionStatementToExpr @@ var "s",
      Cpp._Statement_switch>>: lambda "s" $ switchStatementToExpr @@ var "s",
      Cpp._Statement_iteration>>: lambda "i" $ iterationStatementToExpr @@ var "i",
      Cpp._Statement_jump>>: lambda "j" $ jumpStatementToExpr @@ var "j",
      Cpp._Statement_declaration>>: lambda "v" $ Serialization.withSemi @@ (variableDeclarationToExpr @@ false @@ var "v"),
      Cpp._Statement_expression>>: lambda "e" $ Serialization.withSemi @@ (expressionToExpr @@ var "e")]

subscriptOperationToExpr :: TTermDefinition (Cpp.SubscriptOperation -> Expr)
subscriptOperationToExpr = define "subscriptOperationToExpr" $
  doc "Convert a subscript operation to an expression" $
  lambda "so" $ lets [
    "array">: project Cpp._SubscriptOperation Cpp._SubscriptOperation_array @@ var "so",
    "index">: project Cpp._SubscriptOperation Cpp._SubscriptOperation_index @@ var "so"] $
    Serialization.noSep @@ list [
      postfixExpressionToExpr @@ var "array",
      Serialization.cst @@ string "[",
      expressionToExpr @@ var "index",
      Serialization.cst @@ string "]"]

subtractOperationToExpr :: TTermDefinition (Cpp.SubtractOperation -> Expr)
subtractOperationToExpr = define "subtractOperationToExpr" $
  doc "Convert a subtract operation to an expression" $
  lambda "op" $ lets [
    "left">: project Cpp._SubtractOperation Cpp._SubtractOperation_left @@ var "op",
    "right">: project Cpp._SubtractOperation Cpp._SubtractOperation_right @@ var "op"] $
    Serialization.spaceSep @@ list [
      additiveExpressionToExpr @@ var "left",
      Serialization.cst @@ string "-",
      multiplicativeExpressionToExpr @@ var "right"]

switchStatementToExpr :: TTermDefinition (Cpp.SwitchStatement -> Expr)
switchStatementToExpr = define "switchStatementToExpr" $
  doc "Convert a switch statement to an expression" $
  lambda "ss" $ lets [
    "value">: project Cpp._SwitchStatement Cpp._SwitchStatement_value @@ var "ss",
    "cases">: project Cpp._SwitchStatement Cpp._SwitchStatement_cases @@ var "ss"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "switch",
      Serialization.parens @@ (expressionToExpr @@ var "value"),
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
        (Serialization.newlineSep @@ (Lists.map caseStatementToExpr (var "cases")))]

templateArgumentToExpr :: TTermDefinition (Cpp.TemplateArgument -> Expr)
templateArgumentToExpr = define "templateArgumentToExpr" $
  doc "Convert a template argument to an expression" $
  lambda "a" $
    cases Cpp._TemplateArgument (var "a") Nothing [
      Cpp._TemplateArgument_type>>: lambda "t" $ typeExpressionToExpr @@ var "t",
      Cpp._TemplateArgument_value>>: lambda "e" $ expressionToExpr @@ var "e"]

templateDeclarationToExpr :: TTermDefinition (Cpp.TemplateDeclaration -> Expr)
templateDeclarationToExpr = define "templateDeclarationToExpr" $
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
      declarationToExpr @@ var "declaration"]

templateFunctionCallOperationToExpr :: TTermDefinition (Cpp.TemplateFunctionCallOperation -> Expr)
templateFunctionCallOperationToExpr = define "templateFunctionCallOperationToExpr" $
  doc "Convert a template function call operation to an expression" $
  lambda "tfco" $ lets [
    "func">: project Cpp._TemplateFunctionCallOperation Cpp._TemplateFunctionCallOperation_function @@ var "tfco",
    "templateArgs">: project Cpp._TemplateFunctionCallOperation Cpp._TemplateFunctionCallOperation_templateArguments @@ var "tfco",
    "args">: project Cpp._TemplateFunctionCallOperation Cpp._TemplateFunctionCallOperation_arguments @@ var "tfco"] $
    Serialization.noSep @@ list [
      postfixExpressionToExpr @@ var "func",
      Serialization.angleBracesList @@ Serialization.inlineStyle @@ (Lists.map templateArgumentToExpr (var "templateArgs")),
      Serialization.parenListAdaptive @@ (Lists.map expressionToExpr (var "args"))]

templateTypeToExpr :: TTermDefinition (Cpp.TemplateType -> Expr)
templateTypeToExpr = define "templateTypeToExpr" $
  doc "Convert a template type to an expression" $
  lambda "tt" $ lets [
    "name">: project Cpp._TemplateType Cpp._TemplateType_name @@ var "tt",
    "args">: project Cpp._TemplateType Cpp._TemplateType_arguments @@ var "tt"] $
    Serialization.noSep @@ list [
      Serialization.cst @@ var "name",
      Serialization.angleBracesList @@ Serialization.inlineStyle @@ (Lists.map templateArgumentToExpr (var "args"))]

ternaryExpressionToExpr :: TTermDefinition (Cpp.TernaryExpression -> Expr)
ternaryExpressionToExpr = define "ternaryExpressionToExpr" $
  doc "Convert a ternary expression to an expression" $
  lambda "te" $ lets [
    "cond">: project Cpp._TernaryExpression Cpp._TernaryExpression_condition @@ var "te",
    "trueExpr">: project Cpp._TernaryExpression Cpp._TernaryExpression_trueExpr @@ var "te",
    "falseExpr">: project Cpp._TernaryExpression Cpp._TernaryExpression_falseExpr @@ var "te"] $
    Serialization.spaceSep @@ list [
      logicalOrExpressionToExpr @@ var "cond",
      Serialization.cst @@ string "?",
      expressionToExpr @@ var "trueExpr",
      Serialization.cst @@ string ":",
      conditionalExpressionToExpr @@ var "falseExpr"]

toCppCommentsToExpr :: TTermDefinition (String -> Bool -> String)
toCppCommentsToExpr = define "toCppComments" $
  doc ("Convert a string to a C++ comment. Empty single-line comments emit `//`"
    <> " (no trailing space).") $
  lambda "s" $ lambda "isMultiline" $
    Logic.ifElse (var "isMultiline")
      (Strings.cat $ list [string "/* ", var "s", string " */"])
      (Logic.ifElse (Equality.equal (var "s") (string ""))
        (string "//")
        (Strings.cat2 (string "// ") (var "s")))

typeExpressionToExpr :: TTermDefinition (Cpp.TypeExpression -> Expr)
typeExpressionToExpr = define "typeExpressionToExpr" $
  doc "Convert a type expression to an expression" $
  lambda "t" $
    cases Cpp._TypeExpression (var "t") Nothing [
      Cpp._TypeExpression_basic>>: lambda "b" $ basicTypeToExpr @@ var "b",
      Cpp._TypeExpression_qualified>>: lambda "q" $ qualifiedTypeToExpr @@ var "q",
      Cpp._TypeExpression_template>>: lambda "t" $ templateTypeToExpr @@ var "t",
      Cpp._TypeExpression_function>>: lambda "f" $ functionTypeToExpr @@ var "f",
      Cpp._TypeExpression_auto>>: constant $ Serialization.cst @@ string "auto"]

typedefDeclarationToExpr :: TTermDefinition (Cpp.TypedefDeclaration -> Expr)
typedefDeclarationToExpr = define "typedefDeclarationToExpr" $
  doc "Convert a typedef declaration to an expression" $
  lambda "td" $ lets [
    "name">: project Cpp._TypedefDeclaration Cpp._TypedefDeclaration_name @@ var "td",
    "typ">: project Cpp._TypedefDeclaration Cpp._TypedefDeclaration_type @@ var "td",
    "isUsing">: project Cpp._TypedefDeclaration Cpp._TypedefDeclaration_isUsing @@ var "td"] $
    Logic.ifElse (var "isUsing")
      (Serialization.withSemi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ (Strings.cat2 (string "using ") (var "name")),
        Serialization.cst @@ string "=",
        typeExpressionToExpr @@ var "typ"]))
      (Serialization.withSemi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "typedef",
        typeExpressionToExpr @@ var "typ",
        Serialization.cst @@ var "name"]))

unaryExpressionToExpr :: TTermDefinition (Cpp.UnaryExpression -> Expr)
unaryExpressionToExpr = define "unaryExpressionToExpr" $
  doc "Convert a unary expression to an expression" $
  lambda "e" $
    cases Cpp._UnaryExpression (var "e") Nothing [
      Cpp._UnaryExpression_postfix>>: lambda "p" $ postfixExpressionToExpr @@ var "p",
      Cpp._UnaryExpression_unaryOp>>: lambda "o" $ unaryOperationToExpr @@ var "o",
      Cpp._UnaryExpression_sizeof>>: lambda "s" $ sizeofExpressionToExpr @@ var "s"]

unaryOperationToExpr :: TTermDefinition (Cpp.UnaryOperation -> Expr)
unaryOperationToExpr = define "unaryOperationToExpr" $
  doc "Convert a unary operation to an expression" $
  lambda "uo" $ lets [
    "op">: project Cpp._UnaryOperation Cpp._UnaryOperation_operator @@ var "uo",
    "operand">: project Cpp._UnaryOperation Cpp._UnaryOperation_operand @@ var "uo"] $
    Serialization.spaceSep @@ list [
      unaryOperatorToExpr @@ var "op",
      unaryExpressionToExpr @@ var "operand"]

unaryOperatorToExpr :: TTermDefinition (Cpp.UnaryOperator -> Expr)
unaryOperatorToExpr = define "unaryOperatorToExpr" $
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

undefDirectiveToExpr :: TTermDefinition (Cpp.UndefDirective -> Expr)
undefDirectiveToExpr = define "undefDirectiveToExpr" $
  doc "Convert an undef directive to an expression" $
  lambda "ud" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#undef",
      Serialization.cst @@ (project Cpp._UndefDirective Cpp._UndefDirective_name @@ var "ud")]

variableDeclarationToExpr :: TTermDefinition (Bool -> Cpp.VariableDeclaration -> Expr)
variableDeclarationToExpr = define "variableDeclarationToExpr" $
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
          (lambda "t" $ list [typeExpressionToExpr @@ var "t"])
          (var "typ")),
      list [Serialization.cst @@ var "name"],
      Maybes.maybe
        (list ([] :: [TTerm Expr]))
        (lambda "expr" $ list [Serialization.cst @@ string "=", expressionToExpr @@ var "expr"])
        (var "init")]))

vectorToExpr :: TTermDefinition (Cpp.Vector -> Expr)
vectorToExpr = define "vectorToExpr" $
  doc "Convert a vector to an expression" $
  lambda "v" $ lets [
    "elemType">: project Cpp._Vector Cpp._Vector_elementType @@ var "v",
    "elems">: project Cpp._Vector Cpp._Vector_elements @@ var "v"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "std::vector<",
      typeExpressionToExpr @@ var "elemType",
      Serialization.cst @@ string ">",
      Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ (Lists.map expressionToExpr (var "elems"))]

visitorToExpr :: TTermDefinition (Cpp.Visitor -> Expr)
visitorToExpr = define "visitorToExpr" $
  doc "Convert a visitor to an expression" $
  lambda "v" $
    cases Cpp._Visitor (var "v") Nothing [
      Cpp._Visitor_lambda>>: lambda "l" $ lambdaExpressionToExpr @@ var "l",
      Cpp._Visitor_overloaded>>: lambda "o" $ overloadedLambdasToExpr @@ var "o"]

warningDirectiveToExpr :: TTermDefinition (Cpp.WarningDirective -> Expr)
warningDirectiveToExpr = define "warningDirectiveToExpr" $
  doc "Convert a warning directive to an expression" $
  lambda "wd" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "#warning",
      Serialization.cst @@ (project Cpp._WarningDirective Cpp._WarningDirective_message @@ var "wd")]

whileStatementToExpr :: TTermDefinition (Cpp.WhileStatement -> Expr)
whileStatementToExpr = define "whileStatementToExpr" $
  doc "Convert a while statement to an expression" $
  lambda "ws" $ lets [
    "cond">: project Cpp._WhileStatement Cpp._WhileStatement_condition @@ var "ws",
    "body">: project Cpp._WhileStatement Cpp._WhileStatement_body @@ var "ws"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "while",
        Serialization.parens @@ (expressionToExpr @@ var "cond")],
      statementToExpr @@ var "body"]

