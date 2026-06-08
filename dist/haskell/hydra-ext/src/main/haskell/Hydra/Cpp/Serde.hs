-- Note: this is an automatically generated file. Do not edit.
-- | Serialization functions for converting C++ AST to abstract expressions

module Hydra.Cpp.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Cpp.Syntax as Syntax
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert an access specifier to an expression
accessSpecifierToExpr :: Syntax.AccessSpecifier -> Ast.Expr
accessSpecifierToExpr a =
    case a of
      Syntax.AccessSpecifierPublic -> Serialization.cst "public"
      Syntax.AccessSpecifierProtected -> Serialization.cst "protected"
      Syntax.AccessSpecifierPrivate -> Serialization.cst "private"
      Syntax.AccessSpecifierNone -> Serialization.cst ""
-- | Convert an add operation to an expression
addOperationToExpr :: Syntax.AddOperation -> Ast.Expr
addOperationToExpr op =

      let left = Syntax.addOperationLeft op
          right = Syntax.addOperationRight op
      in (Serialization.spaceSep [
        additiveExpressionToExpr left,
        (Serialization.cst "+"),
        (multiplicativeExpressionToExpr right)])
-- | Convert an additive expression to an expression
additiveExpressionToExpr :: Syntax.AdditiveExpression -> Ast.Expr
additiveExpressionToExpr e =
    case e of
      Syntax.AdditiveExpressionMultiplicative v0 -> multiplicativeExpressionToExpr v0
      Syntax.AdditiveExpressionAdd v0 -> addOperationToExpr v0
      Syntax.AdditiveExpressionSubtract v0 -> subtractOperationToExpr v0
-- | Convert an and expression to an expression
andExpressionToExpr :: Syntax.AndExpression -> Ast.Expr
andExpressionToExpr e =
    case e of
      Syntax.AndExpressionEquality v0 -> equalityExpressionToExpr v0
      Syntax.AndExpressionBitwiseAnd v0 -> bitwiseAndOperationToExpr v0
-- | Convert an assignment expression to an expression
assignmentExpressionToExpr :: Syntax.AssignmentExpression -> Ast.Expr
assignmentExpressionToExpr a =
    case a of
      Syntax.AssignmentExpressionConditional v0 -> conditionalExpressionToExpr v0
      Syntax.AssignmentExpressionAssignment v0 -> explicitAssignmentToExpr v0
-- | Convert an assignment operator to an expression
assignmentOperatorToExpr :: Syntax.AssignmentOperator -> Ast.Expr
assignmentOperatorToExpr op =
    case op of
      Syntax.AssignmentOperatorAssign -> Serialization.cst "="
      Syntax.AssignmentOperatorPlusAssign -> Serialization.cst "+="
      Syntax.AssignmentOperatorMinusAssign -> Serialization.cst "-="
      Syntax.AssignmentOperatorMultiplyAssign -> Serialization.cst "*="
      Syntax.AssignmentOperatorDivideAssign -> Serialization.cst "/="
      Syntax.AssignmentOperatorModuloAssign -> Serialization.cst "%="
      Syntax.AssignmentOperatorLeftShiftAssign -> Serialization.cst "<<="
      Syntax.AssignmentOperatorRightShiftAssign -> Serialization.cst ">>="
      Syntax.AssignmentOperatorBitwiseAndAssign -> Serialization.cst "&="
      Syntax.AssignmentOperatorBitwiseXorAssign -> Serialization.cst "^="
      Syntax.AssignmentOperatorBitwiseOrAssign -> Serialization.cst "|="
-- | Convert a base specifier to an expression
baseSpecifierToExpr :: Syntax.BaseSpecifier -> Ast.Expr
baseSpecifierToExpr bs =

      let access = Syntax.baseSpecifierAccess bs
          name = Syntax.baseSpecifierName bs
      in (Serialization.spaceSep [
        accessSpecifierToExpr access,
        (Serialization.cst name)])
-- | Convert a basic type to an expression
basicTypeToExpr :: Syntax.BasicType -> Ast.Expr
basicTypeToExpr t =
    case t of
      Syntax.BasicTypeVoid -> Serialization.cst "void"
      Syntax.BasicTypeBool -> Serialization.cst "bool"
      Syntax.BasicTypeChar -> Serialization.cst "char"
      Syntax.BasicTypeInt -> Serialization.cst "int"
      Syntax.BasicTypeFloat -> Serialization.cst "float"
      Syntax.BasicTypeDouble -> Serialization.cst "double"
      Syntax.BasicTypeString -> Serialization.cst "std::string"
      Syntax.BasicTypeAuto -> Serialization.cst "auto"
      Syntax.BasicTypeNamed v0 -> Serialization.cst v0
-- | Convert a bitwise and operation to an expression
bitwiseAndOperationToExpr :: Syntax.BitwiseAndOperation -> Ast.Expr
bitwiseAndOperationToExpr op =

      let left = Syntax.bitwiseAndOperationLeft op
          right = Syntax.bitwiseAndOperationRight op
      in (Serialization.spaceSep [
        andExpressionToExpr left,
        (Serialization.cst "&"),
        (equalityExpressionToExpr right)])
-- | Convert a bitwise or operation to an expression
bitwiseOrOperationToExpr :: Syntax.BitwiseOrOperation -> Ast.Expr
bitwiseOrOperationToExpr op =

      let left = Syntax.bitwiseOrOperationLeft op
          right = Syntax.bitwiseOrOperationRight op
      in (Serialization.spaceSep [
        inclusiveOrExpressionToExpr left,
        (Serialization.cst "|"),
        (exclusiveOrExpressionToExpr right)])
-- | Convert a bitwise xor operation to an expression
bitwiseXorOperationToExpr :: Syntax.BitwiseXorOperation -> Ast.Expr
bitwiseXorOperationToExpr op =

      let left = Syntax.bitwiseXorOperationLeft op
          right = Syntax.bitwiseXorOperationRight op
      in (Serialization.spaceSep [
        exclusiveOrExpressionToExpr left,
        (Serialization.cst "^"),
        (andExpressionToExpr right)])
-- | Convert a boolean literal to an expression
booleanLiteralToExpr :: Syntax.BooleanLiteral -> Ast.Expr
booleanLiteralToExpr bl = Logic.ifElse (Syntax.unBooleanLiteral bl) (Serialization.cst "true") (Serialization.cst "false")
-- | Convert a capture list to an expression
captureListToExpr :: Syntax.CaptureList -> Ast.Expr
captureListToExpr cl =
    case cl of
      Syntax.CaptureListCaptureByValue -> Serialization.cst "[=]"
      Syntax.CaptureListCaptures v0 -> Serialization.bracketList Serialization.inlineStyle (Lists.map captureToExpr v0)
-- | Convert a capture to an expression
captureToExpr :: Syntax.Capture -> Ast.Expr
captureToExpr cap =

      let name = Syntax.captureName cap
          byRef = Syntax.captureByReference cap
      in (Logic.ifElse byRef (Serialization.cst (Strings.cat2 "&" name)) (Serialization.cst name))
-- | Convert a case statement to an expression
caseStatementToExpr :: Syntax.CaseStatement -> Ast.Expr
caseStatementToExpr stmt =
    case stmt of
      Syntax.CaseStatementCase v0 -> caseValueToExpr v0
      Syntax.CaseStatementDefault v0 -> Serialization.spaceSep [
        Serialization.cst "default:",
        (statementToExpr v0)]
-- | Convert a case value to an expression
caseValueToExpr :: Syntax.CaseValue -> Ast.Expr
caseValueToExpr cv =

      let value = Syntax.caseValueValue cv
          statement = Syntax.caseValueStatement cv
      in (Serialization.spaceSep [
        Serialization.cst "case",
        (Serialization.noSep [
          expressionToExpr value,
          (Serialization.cst ":")]),
        (statementToExpr statement)])
-- | Convert a class body to an expression
classBodyToExpr :: Bool -> Syntax.ClassBody -> Ast.Expr
classBodyToExpr commas cb =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map (memberSpecificationToExpr commas) (Syntax.unClassBody cb)))
-- | Convert a class declaration to an expression
classDeclarationToExpr :: Syntax.ClassDeclaration -> Ast.Expr
classDeclarationToExpr cd =

      let spec = Syntax.classDeclarationSpecifier cd
          mbody = Syntax.classDeclarationBody cd
          key = Syntax.classSpecifierKey spec
          isEnum = Logic.or (Equality.equal key Syntax.ClassKeyEnum) (Equality.equal key Syntax.ClassKeyEnumClass)
      in (Serialization.withSemi (Serialization.spaceSep (Optionals.cat [
        Just (classSpecifierToExpr spec),
        (Optionals.map (\body -> classBodyToExpr isEnum body) mbody)])))
-- | Convert a class key to an expression
classKeyToExpr :: Syntax.ClassKey -> Ast.Expr
classKeyToExpr k =
    case k of
      Syntax.ClassKeyClass -> Serialization.cst "class"
      Syntax.ClassKeyEnum -> Serialization.cst "enum"
      Syntax.ClassKeyEnumClass -> Serialization.cst "enum class"
      Syntax.ClassKeyStruct -> Serialization.cst "struct"
-- | Convert a class specifier to an expression
classSpecifierToExpr :: Syntax.ClassSpecifier -> Ast.Expr
classSpecifierToExpr cs =

      let key = Syntax.classSpecifierKey cs
          name = Syntax.classSpecifierName cs
          inheritance = Syntax.classSpecifierInheritance cs
      in (Serialization.spaceSep (Lists.concat [
        [
          classKeyToExpr key,
          (Serialization.cst name)],
        (Logic.ifElse (Lists.null inheritance) [] [
          Serialization.cst ":",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map baseSpecifierToExpr inheritance))])]))
-- | Convert a comma expression to an expression
commaExpressionToExpr :: Syntax.CommaExpression -> Ast.Expr
commaExpressionToExpr ce =

      let left = Syntax.commaExpressionLeft ce
          right = Syntax.commaExpressionRight ce
      in (Serialization.spaceSep [
        expressionToExpr left,
        (Serialization.cst ","),
        (assignmentExpressionToExpr right)])
-- | Convert a comment to an expression
commentToExpr :: Syntax.Comment -> Ast.Expr
commentToExpr c =

      let text = Syntax.commentText c
          isMultiline = Syntax.commentIsMultiline c
      in (Serialization.cst (toCppComments text isMultiline))
-- | Convert a compound statement to an expression
compoundStatementToExpr :: Syntax.CompoundStatement -> Ast.Expr
compoundStatementToExpr cs =
    Serialization.curlyBracesList (Just "") Serialization.fullBlockStyle (Lists.map statementToExpr (Syntax.unCompoundStatement cs))
-- | Convert a conditional expression to an expression
conditionalExpressionToExpr :: Syntax.ConditionalExpression -> Ast.Expr
conditionalExpressionToExpr c =
    case c of
      Syntax.ConditionalExpressionLogicalOr v0 -> logicalOrExpressionToExpr v0
      Syntax.ConditionalExpressionTernary v0 -> ternaryExpressionToExpr v0
-- | Convert a constructor declaration to an expression
constructorDeclarationToExpr :: Syntax.ConstructorDeclaration -> Ast.Expr
constructorDeclarationToExpr cd =

      let name = Syntax.constructorDeclarationName cd
          params = Syntax.constructorDeclarationParameters cd
          inits = Syntax.constructorDeclarationInitializers cd
          body = Syntax.constructorDeclarationBody cd
      in (Serialization.spaceSep (Optionals.cat [
        Just (Serialization.noSep [
          Serialization.cst name,
          (Serialization.parenListAdaptive (Lists.map parameterToExpr params))]),
        (Logic.ifElse (Lists.null inits) Nothing (Just (Serialization.spaceSep [
          Serialization.cst ":",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map memInitializerToExpr inits))]))),
        (Just (functionBodyToExpr body))]))
-- | Convert a declaration to an expression
declarationToExpr :: Syntax.Declaration -> Ast.Expr
declarationToExpr d =
    case d of
      Syntax.DeclarationPreprocessor v0 -> preprocessorDirectiveToExpr v0
      Syntax.DeclarationClass v0 -> classDeclarationToExpr v0
      Syntax.DeclarationFunction v0 -> functionDeclarationToExpr v0
      Syntax.DeclarationVariable v0 -> variableDeclarationToExpr False v0
      Syntax.DeclarationTypedef v0 -> typedefDeclarationToExpr v0
      Syntax.DeclarationNamespace v0 -> namespaceDeclarationToExpr v0
      Syntax.DeclarationTemplate v0 -> templateDeclarationToExpr v0
-- | Convert a define directive to an expression
defineDirectiveToExpr :: Syntax.DefineDirective -> Ast.Expr
defineDirectiveToExpr dd =

      let name = Syntax.defineDirectiveName dd
          params = Syntax.defineDirectiveParameters dd
          replacement = Syntax.defineDirectiveReplacement dd
      in (Serialization.spaceSep (Lists.concat [
        [
          Serialization.cst "#define",
          (Serialization.cst name)],
        (Optionals.cases params [] (\ps -> [
          Serialization.parenListAdaptive (Lists.map (\p -> Serialization.cst p) ps)])),
        (Optionals.cases replacement [] (\r -> [
          Serialization.cst r]))]))
-- | Convert a destructor declaration to an expression
destructorDeclarationToExpr :: Syntax.DestructorDeclaration -> Ast.Expr
destructorDeclarationToExpr dd =

      let prefixSpecs = Syntax.destructorDeclarationPrefixSpecifiers dd
          name = Syntax.destructorDeclarationName dd
          suffixSpecs = Syntax.destructorDeclarationSuffixSpecifiers dd
          body = Syntax.destructorDeclarationBody dd
      in (Serialization.spaceSep (Lists.concat [
        Lists.map functionSpecifierPrefixToExpr prefixSpecs,
        [
          Serialization.noSep [
            Serialization.cst (Strings.cat2 "~" name),
            (Serialization.parens (Serialization.cst ""))]],
        (Lists.map functionSpecifierSuffixToExpr suffixSpecs),
        [
          functionBodyToExpr body]]))
-- | Convert a divide operation to an expression
divideOperationToExpr :: Syntax.DivideOperation -> Ast.Expr
divideOperationToExpr op =

      let left = Syntax.divideOperationLeft op
          right = Syntax.divideOperationRight op
      in (Serialization.spaceSep [
        multiplicativeExpressionToExpr left,
        (Serialization.cst "/"),
        (unaryExpressionToExpr right)])
-- | Convert a do statement to an expression
doStatementToExpr :: Syntax.DoStatement -> Ast.Expr
doStatementToExpr ds =

      let body = Syntax.doStatementBody ds
          cond = Syntax.doStatementCondition ds
      in (Serialization.newlineSep [
        Serialization.cst "do",
        (statementToExpr body),
        (Serialization.withSemi (Serialization.spaceSep [
          Serialization.cst "while",
          (Serialization.parens (expressionToExpr cond))]))])
-- | Convert an elif directive to an expression
elifDirectiveToExpr :: Syntax.ElifDirective -> Ast.Expr
elifDirectiveToExpr ed =
    Serialization.spaceSep [
      Serialization.cst "#elif",
      (Serialization.cst (Syntax.elifDirectiveCondition ed))]
-- | Convert an else directive to an expression
elseDirectiveToExpr :: t0 -> Ast.Expr
elseDirectiveToExpr ed = Serialization.cst "#else"
-- | Convert an endif directive to an expression
endifDirectiveToExpr :: t0 -> Ast.Expr
endifDirectiveToExpr ed = Serialization.cst "#endif"
-- | Convert an equal operation to an expression
equalOperationToExpr :: Syntax.EqualOperation -> Ast.Expr
equalOperationToExpr op =

      let left = Syntax.equalOperationLeft op
          right = Syntax.equalOperationRight op
      in (Serialization.spaceSep [
        equalityExpressionToExpr left,
        (Serialization.cst "=="),
        (relationalExpressionToExpr right)])
-- | Convert an equality expression to an expression
equalityExpressionToExpr :: Syntax.EqualityExpression -> Ast.Expr
equalityExpressionToExpr e =
    case e of
      Syntax.EqualityExpressionRelational v0 -> relationalExpressionToExpr v0
      Syntax.EqualityExpressionEqual v0 -> equalOperationToExpr v0
      Syntax.EqualityExpressionNotEqual v0 -> notEqualOperationToExpr v0
-- | Convert an error directive to an expression
errorDirectiveToExpr :: Syntax.ErrorDirective -> Ast.Expr
errorDirectiveToExpr ed =
    Serialization.spaceSep [
      Serialization.cst "#error",
      (Serialization.cst (Syntax.errorDirectiveMessage ed))]
-- | Convert an exclusive or expression to an expression
exclusiveOrExpressionToExpr :: Syntax.ExclusiveOrExpression -> Ast.Expr
exclusiveOrExpressionToExpr e =
    case e of
      Syntax.ExclusiveOrExpressionAnd v0 -> andExpressionToExpr v0
      Syntax.ExclusiveOrExpressionBitwiseXor v0 -> bitwiseXorOperationToExpr v0
-- | Convert an explicit assignment to an expression
explicitAssignmentToExpr :: Syntax.ExplicitAssignment -> Ast.Expr
explicitAssignmentToExpr ea =

      let left = Syntax.explicitAssignmentLeft ea
          op = Syntax.explicitAssignmentOp ea
          right = Syntax.explicitAssignmentRight ea
      in (Serialization.spaceSep [
        logicalOrExpressionToExpr left,
        (assignmentOperatorToExpr op),
        (assignmentExpressionToExpr right)])
-- | Convert an expression to an expression
expressionToExpr :: Syntax.Expression -> Ast.Expr
expressionToExpr e =
    case e of
      Syntax.ExpressionAssignment v0 -> assignmentExpressionToExpr v0
      Syntax.ExpressionComma v0 -> commaExpressionToExpr v0
-- | Convert a for-init to an expression
forInitToExpr :: Syntax.ForInit -> Ast.Expr
forInitToExpr i =
    case i of
      Syntax.ForInitExpression v0 -> expressionToExpr v0
      Syntax.ForInitDeclaration v0 -> variableDeclarationToExpr False v0
      Syntax.ForInitEmpty -> Serialization.cst ""
-- | Convert a for statement to an expression
forStatementToExpr :: Syntax.ForStatement -> Ast.Expr
forStatementToExpr fs =

      let init = Syntax.forStatementInit fs
          cond = Syntax.forStatementCondition fs
          inc = Syntax.forStatementIncrement fs
          body = Syntax.forStatementBody fs
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "for",
          (Serialization.parens (Serialization.noSep [
            forInitToExpr init,
            (Serialization.cst ";"),
            (expressionToExpr cond),
            (Serialization.cst ";"),
            (expressionToExpr inc)]))],
        (statementToExpr body)])
-- | Convert a function application to an expression
functionApplicationToExpr :: Syntax.FunctionApplication -> Ast.Expr
functionApplicationToExpr fa =

      let func = Syntax.functionApplicationFunction fa
          args = Syntax.functionApplicationArguments fa
      in (Serialization.spaceSep [
        functionIdentifierToExpr func,
        (Serialization.parenListAdaptive (Lists.map expressionToExpr args))])
-- | Convert a function body to an expression
functionBodyToExpr :: Syntax.FunctionBody -> Ast.Expr
functionBodyToExpr b =
    case b of
      Syntax.FunctionBodyCompound v0 -> compoundStatementToExpr v0
      Syntax.FunctionBodyDeclaration -> Serialization.cst ";"
      Syntax.FunctionBodyPure -> Serialization.withSemi (Serialization.cst "= 0")
      Syntax.FunctionBodyDefault -> Serialization.withSemi (Serialization.cst "= default")
-- | Convert a function call operation to an expression
functionCallOperationToExpr :: Syntax.FunctionCallOperation -> Ast.Expr
functionCallOperationToExpr fco =

      let func = Syntax.functionCallOperationFunction fco
          args = Syntax.functionCallOperationArguments fco
      in (Serialization.noSep [
        postfixExpressionToExpr func,
        (Serialization.parenListAdaptive (Lists.map expressionToExpr args))])
-- | Convert a function declaration to an expression
functionDeclarationToExpr :: Syntax.FunctionDeclaration -> Ast.Expr
functionDeclarationToExpr fd =

      let prefixSpecs = Syntax.functionDeclarationPrefixSpecifiers fd
          retType = Syntax.functionDeclarationReturnType fd
          name = Syntax.functionDeclarationName fd
          params = Syntax.functionDeclarationParameters fd
          suffixSpecs = Syntax.functionDeclarationSuffixSpecifiers fd
          body = Syntax.functionDeclarationBody fd
      in (Serialization.spaceSep (Lists.concat [
        Lists.map functionSpecifierPrefixToExpr prefixSpecs,
        [
          typeExpressionToExpr retType,
          (Serialization.noSep [
            Serialization.cst name,
            (Serialization.parenListAdaptive (Lists.map parameterToExpr params))])],
        (Lists.map functionSpecifierSuffixToExpr suffixSpecs),
        [
          functionBodyToExpr body]]))
-- | Convert a function identifier to an expression
functionIdentifierToExpr :: Syntax.FunctionIdentifier -> Ast.Expr
functionIdentifierToExpr f =
    case f of
      Syntax.FunctionIdentifierSimple v0 -> Serialization.cst v0
      Syntax.FunctionIdentifierQualified v0 -> qualifiedIdentifierToExpr v0
-- | Convert a function specifier prefix to an expression
functionSpecifierPrefixToExpr :: Syntax.FunctionSpecifierPrefix -> Ast.Expr
functionSpecifierPrefixToExpr s =
    case s of
      Syntax.FunctionSpecifierPrefixInline -> Serialization.cst "inline"
      Syntax.FunctionSpecifierPrefixVirtual -> Serialization.cst "virtual"
      Syntax.FunctionSpecifierPrefixStatic -> Serialization.cst "static"
      Syntax.FunctionSpecifierPrefixExplicit -> Serialization.cst "explicit"
-- | Convert a function specifier suffix to an expression
functionSpecifierSuffixToExpr :: Syntax.FunctionSpecifierSuffix -> Ast.Expr
functionSpecifierSuffixToExpr s =
    case s of
      Syntax.FunctionSpecifierSuffixConst -> Serialization.cst "const"
      Syntax.FunctionSpecifierSuffixNoexcept -> Serialization.cst "noexcept"
      Syntax.FunctionSpecifierSuffixOverride -> Serialization.cst "override"
      Syntax.FunctionSpecifierSuffixFinal -> Serialization.cst "final"
-- | Convert a function type to an expression
functionTypeToExpr :: Syntax.FunctionType -> Ast.Expr
functionTypeToExpr ft =

      let retType = Syntax.functionTypeReturnType ft
          params = Syntax.functionTypeParameters ft
      in (Serialization.spaceSep [
        typeExpressionToExpr retType,
        (Serialization.parenListAdaptive (Lists.map parameterToExpr params))])
-- | Convert a greater-than-or-equal operation to an expression
greaterEqualOperationToExpr :: Syntax.GreaterEqualOperation -> Ast.Expr
greaterEqualOperationToExpr op =

      let left = Syntax.greaterEqualOperationLeft op
          right = Syntax.greaterEqualOperationRight op
      in (Serialization.spaceSep [
        relationalExpressionToExpr left,
        (Serialization.cst ">="),
        (shiftExpressionToExpr right)])
-- | Convert a greater-than operation to an expression
greaterOperationToExpr :: Syntax.GreaterOperation -> Ast.Expr
greaterOperationToExpr op =

      let left = Syntax.greaterOperationLeft op
          right = Syntax.greaterOperationRight op
      in (Serialization.spaceSep [
        relationalExpressionToExpr left,
        (Serialization.cst ">"),
        (shiftExpressionToExpr right)])
-- | Convert an if directive to an expression
ifDirectiveToExpr :: Syntax.IfDirective -> Ast.Expr
ifDirectiveToExpr ifd =
    Serialization.spaceSep [
      Serialization.cst "#if",
      (Serialization.cst (Syntax.ifDirectiveCondition ifd))]
-- | Convert an ifdef directive to an expression
ifdefDirectiveToExpr :: Syntax.IfdefDirective -> Ast.Expr
ifdefDirectiveToExpr id =
    Serialization.spaceSep [
      Serialization.cst "#ifdef",
      (Serialization.cst (Syntax.ifdefDirectiveIdentifier id))]
-- | Convert an ifndef directive to an expression
ifndefDirectiveToExpr :: Syntax.IfndefDirective -> Ast.Expr
ifndefDirectiveToExpr ind =
    Serialization.spaceSep [
      Serialization.cst "#ifndef",
      (Serialization.cst (Syntax.ifndefDirectiveIdentifier ind))]
-- | Convert an include directive to an expression
includeDirectiveToExpr :: Syntax.IncludeDirective -> Ast.Expr
includeDirectiveToExpr incl =

      let name = Syntax.includeDirectiveName incl
          isSystem = Syntax.includeDirectiveIsSystem incl
      in (Logic.ifElse isSystem (Serialization.cst (Strings.cat [
        "#include <",
        name,
        ">"])) (Serialization.cst (Strings.cat [
        "#include \"",
        name,
        "\""])))
-- | Convert an inclusive or expression to an expression
inclusiveOrExpressionToExpr :: Syntax.InclusiveOrExpression -> Ast.Expr
inclusiveOrExpressionToExpr e =
    case e of
      Syntax.InclusiveOrExpressionExclusiveOr v0 -> exclusiveOrExpressionToExpr v0
      Syntax.InclusiveOrExpressionBitwiseOr v0 -> bitwiseOrOperationToExpr v0
-- | Convert an integer literal to an expression
integerLiteralToExpr :: Syntax.IntegerLiteral -> Ast.Expr
integerLiteralToExpr i =
    case i of
      Syntax.IntegerLiteralDecimal v0 -> Serialization.cst (Literals.showBigint v0)
      Syntax.IntegerLiteralHexadecimal v0 -> Serialization.cst (Strings.cat2 "0x" v0)
      Syntax.IntegerLiteralOctal v0 -> Serialization.cst (Strings.cat2 "0" v0)
      Syntax.IntegerLiteralBinary v0 -> Serialization.cst (Strings.cat2 "0b" v0)
-- | Convert an iteration statement to an expression
iterationStatementToExpr :: Syntax.IterationStatement -> Ast.Expr
iterationStatementToExpr i =
    case i of
      Syntax.IterationStatementWhile v0 -> whileStatementToExpr v0
      Syntax.IterationStatementDo v0 -> doStatementToExpr v0
      Syntax.IterationStatementFor v0 -> forStatementToExpr v0
      Syntax.IterationStatementRangeFor v0 -> rangeForStatementToExpr v0
-- | Convert a jump statement to an expression
jumpStatementToExpr :: Syntax.JumpStatement -> Ast.Expr
jumpStatementToExpr j =
    case j of
      Syntax.JumpStatementBreak -> Serialization.withSemi (Serialization.cst "break")
      Syntax.JumpStatementContinue -> Serialization.withSemi (Serialization.cst "continue")
      Syntax.JumpStatementReturnValue v0 -> Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "return",
        (expressionToExpr v0)])
      Syntax.JumpStatementReturnVoid -> Serialization.withSemi (Serialization.cst "return")
      Syntax.JumpStatementThrow v0 -> Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "throw",
        (expressionToExpr v0)])
-- | Convert a labeled statement to an expression
labeledStatementToExpr :: Syntax.LabeledStatement -> Ast.Expr
labeledStatementToExpr ls =

      let label = Syntax.labeledStatementLabel ls
          stmt = Syntax.labeledStatementStatement ls
      in (Serialization.newlineSep [
        Serialization.cst (Strings.cat2 label ":"),
        (statementToExpr stmt)])
-- | Convert a lambda expression to an expression
lambdaExpressionToExpr :: Syntax.LambdaExpression -> Ast.Expr
lambdaExpressionToExpr le =

      let captures = Syntax.lambdaExpressionCaptures le
          params = Syntax.lambdaExpressionParameters le
          retType = Syntax.lambdaExpressionReturnType le
          body = Syntax.lambdaExpressionBody le
      in (Serialization.spaceSep [
        captureListToExpr captures,
        (Logic.ifElse (Lists.null params) (Serialization.parens (Serialization.cst "")) (Serialization.parenListAdaptive (Lists.map parameterToExpr params))),
        (Optionals.cases retType (Serialization.cst "") (\t -> Serialization.spaceSep [
          Serialization.cst "->",
          (typeExpressionToExpr t)])),
        (compoundStatementToExpr body)])
-- | Convert a left shift operation to an expression
leftShiftOperationToExpr :: Syntax.LeftShiftOperation -> Ast.Expr
leftShiftOperationToExpr op =

      let left = Syntax.leftShiftOperationLeft op
          right = Syntax.leftShiftOperationRight op
      in (Serialization.spaceSep [
        shiftExpressionToExpr left,
        (Serialization.cst "<<"),
        (additiveExpressionToExpr right)])
-- | Convert a less-than-or-equal operation to an expression
lessEqualOperationToExpr :: Syntax.LessEqualOperation -> Ast.Expr
lessEqualOperationToExpr op =

      let left = Syntax.lessEqualOperationLeft op
          right = Syntax.lessEqualOperationRight op
      in (Serialization.spaceSep [
        relationalExpressionToExpr left,
        (Serialization.cst "<="),
        (shiftExpressionToExpr right)])
-- | Convert a less-than operation to an expression
lessOperationToExpr :: Syntax.LessOperation -> Ast.Expr
lessOperationToExpr op =

      let left = Syntax.lessOperationLeft op
          right = Syntax.lessOperationRight op
      in (Serialization.spaceSep [
        relationalExpressionToExpr left,
        (Serialization.cst "<"),
        (shiftExpressionToExpr right)])
-- | Convert a line directive to an expression
lineDirectiveToExpr :: Syntax.LineDirective -> Ast.Expr
lineDirectiveToExpr ld =

      let lineNumber = Syntax.lineDirectiveLineNumber ld
          filename = Syntax.lineDirectiveFilename ld
      in (Serialization.spaceSep (Lists.concat [
        [
          Serialization.cst "#line",
          (Serialization.cst (Literals.showInt32 lineNumber))],
        (Optionals.cases filename [] (\f -> [
          Serialization.cst (Strings.cat [
            "\"",
            f,
            "\""])]))]))
-- | Convert a literal to an expression
literalToExpr :: Syntax.Literal -> Ast.Expr
literalToExpr l =
    case l of
      Syntax.LiteralInteger v0 -> integerLiteralToExpr v0
      Syntax.LiteralFloating v0 -> Serialization.cst (Literals.showFloat64 (Syntax.unFloatingLiteral v0))
      Syntax.LiteralCharacter v0 -> Serialization.cst (Strings.cat [
        "'",
        (Syntax.unCharacterLiteral v0),
        "'"])
      Syntax.LiteralString v0 -> Serialization.cst (Strings.cat [
        "\"",
        (Syntax.unStringLiteral v0),
        "\""])
      Syntax.LiteralBoolean v0 -> booleanLiteralToExpr v0
      Syntax.LiteralNull -> Serialization.cst "nullptr"
-- | Convert a logical and expression to an expression
logicalAndExpressionToExpr :: Syntax.LogicalAndExpression -> Ast.Expr
logicalAndExpressionToExpr e =
    case e of
      Syntax.LogicalAndExpressionInclusiveOr v0 -> inclusiveOrExpressionToExpr v0
      Syntax.LogicalAndExpressionLogicalAnd v0 -> logicalAndOperationToExpr v0
-- | Convert a logical and operation to an expression
logicalAndOperationToExpr :: Syntax.LogicalAndOperation -> Ast.Expr
logicalAndOperationToExpr op =

      let left = Syntax.logicalAndOperationLeft op
          right = Syntax.logicalAndOperationRight op
      in (Serialization.spaceSep [
        logicalAndExpressionToExpr left,
        (Serialization.cst "&&"),
        (inclusiveOrExpressionToExpr right)])
-- | Convert a logical or expression to an expression
logicalOrExpressionToExpr :: Syntax.LogicalOrExpression -> Ast.Expr
logicalOrExpressionToExpr e =
    case e of
      Syntax.LogicalOrExpressionLogicalAnd v0 -> logicalAndExpressionToExpr v0
      Syntax.LogicalOrExpressionLogicalOr v0 -> logicalOrOperationToExpr v0
-- | Convert a logical or operation to an expression
logicalOrOperationToExpr :: Syntax.LogicalOrOperation -> Ast.Expr
logicalOrOperationToExpr op =

      let left = Syntax.logicalOrOperationLeft op
          right = Syntax.logicalOrOperationRight op
      in (Serialization.spaceSep [
        logicalOrExpressionToExpr left,
        (Serialization.cst "||"),
        (logicalAndExpressionToExpr right)])
-- | Convert a map entry to an expression
mapEntryToExpr :: Syntax.MapEntry -> Ast.Expr
mapEntryToExpr me =

      let key = Syntax.mapEntryKey me
          val = Syntax.mapEntryValue me
      in (Serialization.spaceSep [
        Serialization.curlyBracesList Nothing Serialization.inlineStyle [
          expressionToExpr key],
        (Serialization.cst "->"),
        (expressionToExpr val)])
-- | Convert a map to an expression
mapToExpr :: Syntax.Map -> Ast.Expr
mapToExpr m =

      let keyType = Syntax.mapKeyType m
          valType = Syntax.mapValueType m
          entries = Syntax.mapEntries m
      in (Serialization.spaceSep [
        Serialization.cst "std::map<",
        (Serialization.commaSep Serialization.inlineStyle [
          typeExpressionToExpr keyType,
          (typeExpressionToExpr valType)]),
        (Serialization.cst ">"),
        (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map mapEntryToExpr entries))])
-- | Convert a member initializer to an expression
memInitializerToExpr :: Syntax.MemInitializer -> Ast.Expr
memInitializerToExpr mi =

      let name = Syntax.memInitializerName mi
          args = Syntax.memInitializerArguments mi
      in (Serialization.noSep [
        Serialization.cst name,
        (Serialization.parenListAdaptive (Lists.map expressionToExpr args))])
-- | Convert a member access operation to an expression
memberAccessOperationToExpr :: Syntax.MemberAccessOperation -> Ast.Expr
memberAccessOperationToExpr mao =

      let obj = Syntax.memberAccessOperationObject mao
          member = Syntax.memberAccessOperationMember mao
      in (Serialization.noSep [
        postfixExpressionToExpr obj,
        (Serialization.cst "."),
        (Serialization.cst member)])
-- | Convert a member declaration to an expression
memberDeclarationToExpr :: Bool -> Syntax.MemberDeclaration -> Ast.Expr
memberDeclarationToExpr commas m =
    case m of
      Syntax.MemberDeclarationFunction v0 -> functionDeclarationToExpr v0
      Syntax.MemberDeclarationVariable v0 -> variableDeclarationToExpr commas v0
      Syntax.MemberDeclarationConstructor v0 -> constructorDeclarationToExpr v0
      Syntax.MemberDeclarationDestructor v0 -> destructorDeclarationToExpr v0
      Syntax.MemberDeclarationNestedClass v0 -> classDeclarationToExpr v0
      Syntax.MemberDeclarationTemplate v0 -> templateDeclarationToExpr v0
-- | Convert a member specification to an expression
memberSpecificationToExpr :: Bool -> Syntax.MemberSpecification -> Ast.Expr
memberSpecificationToExpr commas m =
    case m of
      Syntax.MemberSpecificationAccessLabel v0 -> Serialization.noSep [
        accessSpecifierToExpr v0,
        (Serialization.cst ":")]
      Syntax.MemberSpecificationMember v0 -> memberDeclarationToExpr commas v0
-- | Convert a modulo operation to an expression
moduloOperationToExpr :: Syntax.ModuloOperation -> Ast.Expr
moduloOperationToExpr op =

      let left = Syntax.moduloOperationLeft op
          right = Syntax.moduloOperationRight op
      in (Serialization.spaceSep [
        multiplicativeExpressionToExpr left,
        (Serialization.cst "%"),
        (unaryExpressionToExpr right)])
-- | Convert a multiplicative expression to an expression
multiplicativeExpressionToExpr :: Syntax.MultiplicativeExpression -> Ast.Expr
multiplicativeExpressionToExpr e =
    case e of
      Syntax.MultiplicativeExpressionUnary v0 -> unaryExpressionToExpr v0
      Syntax.MultiplicativeExpressionMultiply v0 -> multiplyOperationToExpr v0
      Syntax.MultiplicativeExpressionDivide v0 -> divideOperationToExpr v0
      Syntax.MultiplicativeExpressionModulo v0 -> moduloOperationToExpr v0
-- | Convert a multiply operation to an expression
multiplyOperationToExpr :: Syntax.MultiplyOperation -> Ast.Expr
multiplyOperationToExpr op =

      let left = Syntax.multiplyOperationLeft op
          right = Syntax.multiplyOperationRight op
      in (Serialization.spaceSep [
        multiplicativeExpressionToExpr left,
        (Serialization.cst "*"),
        (unaryExpressionToExpr right)])
-- | Convert a namespace declaration to an expression
namespaceDeclarationToExpr :: Syntax.NamespaceDeclaration -> Ast.Expr
namespaceDeclarationToExpr nd =

      let name = Syntax.namespaceDeclarationName nd
          decls = Syntax.namespaceDeclarationDeclarations nd
      in (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "namespace " name),
        (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map declarationToExpr decls)))])
-- | Convert a not-equal operation to an expression
notEqualOperationToExpr :: Syntax.NotEqualOperation -> Ast.Expr
notEqualOperationToExpr op =

      let left = Syntax.notEqualOperationLeft op
          right = Syntax.notEqualOperationRight op
      in (Serialization.spaceSep [
        equalityExpressionToExpr left,
        (Serialization.cst "!="),
        (relationalExpressionToExpr right)])
-- | Convert an optional to an expression
optionalToExpr :: Syntax.Optional -> Ast.Expr
optionalToExpr opt =

      let valType = Syntax.optionalValueType opt
          val = Syntax.optionalValue opt
      in (Serialization.spaceSep [
        Serialization.cst "std::optional<",
        (typeExpressionToExpr valType),
        (Serialization.cst ">"),
        (Optionals.cases val (Serialization.cst "{}") (\v -> Serialization.curlyBracesList Nothing Serialization.inlineStyle [
          expressionToExpr v]))])
-- | Convert overloaded lambdas to an expression
overloadedLambdasToExpr :: Syntax.OverloadedLambdas -> Ast.Expr
overloadedLambdasToExpr ol =
    Serialization.spaceSep [
      Serialization.cst "overloaded",
      (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map lambdaExpressionToExpr (Syntax.unOverloadedLambdas ol))))]
-- | Convert a parameter to an expression
parameterToExpr :: Syntax.Parameter -> Ast.Expr
parameterToExpr p =

      let typ = Syntax.parameterType p
          name = Syntax.parameterName p
          unnamed = Syntax.parameterUnnamed p
          defaultVal = Syntax.parameterDefaultValue p
          nameExpr =
                  Serialization.cst (Logic.ifElse unnamed (Strings.cat [
                    "/*",
                    name,
                    "*/"]) name)
      in (Serialization.spaceSep (Lists.concat [
        [
          typeExpressionToExpr typ,
          nameExpr],
        (Optionals.cases defaultVal [] (\expr -> [
          Serialization.cst "=",
          (expressionToExpr expr)]))]))
-- | Convert a pattern match to an expression
patternMatchToExpr :: Syntax.PatternMatch -> Ast.Expr
patternMatchToExpr pm =

      let visitor = Syntax.patternMatchVisitor pm
          variant = Syntax.patternMatchVariant pm
      in (Serialization.spaceSep [
        Serialization.cst "std::visit",
        (Serialization.parenListAdaptive [
          visitorToExpr visitor,
          (expressionToExpr variant)])])
-- | Convert a pointer member access operation to an expression
pointerMemberAccessOperationToExpr :: Syntax.PointerMemberAccessOperation -> Ast.Expr
pointerMemberAccessOperationToExpr pmao =

      let ptr = Syntax.pointerMemberAccessOperationPointer pmao
          member = Syntax.pointerMemberAccessOperationMember pmao
      in (Serialization.noSep [
        postfixExpressionToExpr ptr,
        (Serialization.cst "->"),
        (Serialization.cst member)])
-- | Convert a postfix expression to an expression
postfixExpressionToExpr :: Syntax.PostfixExpression -> Ast.Expr
postfixExpressionToExpr e =
    case e of
      Syntax.PostfixExpressionPrimary v0 -> primaryExpressionToExpr v0
      Syntax.PostfixExpressionSubscript v0 -> subscriptOperationToExpr v0
      Syntax.PostfixExpressionFunctionCall v0 -> functionCallOperationToExpr v0
      Syntax.PostfixExpressionTemplateFunctionCall v0 -> templateFunctionCallOperationToExpr v0
      Syntax.PostfixExpressionMemberAccess v0 -> memberAccessOperationToExpr v0
      Syntax.PostfixExpressionPointerMemberAccess v0 -> pointerMemberAccessOperationToExpr v0
      Syntax.PostfixExpressionPostIncrement v0 -> Serialization.noSep [
        postfixExpressionToExpr v0,
        (Serialization.cst "++")]
      Syntax.PostfixExpressionPostDecrement v0 -> Serialization.noSep [
        postfixExpressionToExpr v0,
        (Serialization.cst "--")]
-- | Convert a pragma directive to an expression
pragmaDirectiveToExpr :: Syntax.PragmaDirective -> Ast.Expr
pragmaDirectiveToExpr pd = Serialization.cst (Strings.cat2 "#pragma " (Syntax.pragmaDirectiveContent pd))
-- | Convert a preprocessor directive to an expression
preprocessorDirectiveToExpr :: Syntax.PreprocessorDirective -> Ast.Expr
preprocessorDirectiveToExpr d =
    case d of
      Syntax.PreprocessorDirectiveInclude v0 -> includeDirectiveToExpr v0
      Syntax.PreprocessorDirectivePragma v0 -> pragmaDirectiveToExpr v0
      Syntax.PreprocessorDirectiveDefine v0 -> defineDirectiveToExpr v0
      Syntax.PreprocessorDirectiveUndef v0 -> undefDirectiveToExpr v0
      Syntax.PreprocessorDirectiveIfdef v0 -> ifdefDirectiveToExpr v0
      Syntax.PreprocessorDirectiveIfndef v0 -> ifndefDirectiveToExpr v0
      Syntax.PreprocessorDirectiveIf v0 -> ifDirectiveToExpr v0
      Syntax.PreprocessorDirectiveElif v0 -> elifDirectiveToExpr v0
      Syntax.PreprocessorDirectiveElse v0 -> elseDirectiveToExpr v0
      Syntax.PreprocessorDirectiveEndif v0 -> endifDirectiveToExpr v0
      Syntax.PreprocessorDirectiveLine v0 -> lineDirectiveToExpr v0
      Syntax.PreprocessorDirectiveError v0 -> errorDirectiveToExpr v0
      Syntax.PreprocessorDirectiveWarning v0 -> warningDirectiveToExpr v0
-- | Convert a primary expression to an expression
primaryExpressionToExpr :: Syntax.PrimaryExpression -> Ast.Expr
primaryExpressionToExpr e =
    case e of
      Syntax.PrimaryExpressionIdentifier v0 -> Serialization.cst v0
      Syntax.PrimaryExpressionLiteral v0 -> literalToExpr v0
      Syntax.PrimaryExpressionParenthesized v0 -> Serialization.parens (expressionToExpr v0)
      Syntax.PrimaryExpressionLambda v0 -> lambdaExpressionToExpr v0
-- | Convert a program to an expression
programToExpr :: Syntax.Program -> Ast.Expr
programToExpr prog =

      let preps = Syntax.programPreprocessorDirectives prog
          includes = Syntax.programIncludes prog
          decls = Syntax.programDeclarations prog
          separate = \sep -> \defs -> Logic.ifElse (Lists.null defs) Nothing (Just (sep defs))
      in (Serialization.doubleNewlineSep (Optionals.cat [
        separate Serialization.newlineSep (Lists.map preprocessorDirectiveToExpr preps),
        (separate Serialization.newlineSep (Lists.map includeDirectiveToExpr includes)),
        (separate Serialization.doubleNewlineSep (Lists.map declarationToExpr decls))]))
-- | Convert a qualified identifier to an expression
qualifiedIdentifierToExpr :: Syntax.QualifiedIdentifier -> Ast.Expr
qualifiedIdentifierToExpr qi =

      let ns = Syntax.qualifiedIdentifierNamespace qi
          name = Syntax.qualifiedIdentifierName qi
      in (Serialization.cst (Strings.cat [
        ns,
        "::",
        name]))
-- | Convert a qualified type to an expression
qualifiedTypeToExpr :: Syntax.QualifiedType -> Ast.Expr
qualifiedTypeToExpr qt =

      let baseType = Syntax.qualifiedTypeBaseType qt
          qualifier = Syntax.qualifiedTypeQualifier qt
      in case qualifier of
        Syntax.TypeQualifierConst -> Serialization.spaceSep [
          Serialization.cst "const",
          (typeExpressionToExpr baseType)]
        Syntax.TypeQualifierLvalueRef -> Serialization.noSep [
          typeExpressionToExpr baseType,
          (Serialization.cst "&")]
        Syntax.TypeQualifierRvalueRef -> Serialization.noSep [
          typeExpressionToExpr baseType,
          (Serialization.cst "&&")]
        Syntax.TypeQualifierPointer -> Serialization.noSep [
          typeExpressionToExpr baseType,
          (Serialization.cst "*")]
-- | Convert a range-for statement to an expression
rangeForStatementToExpr :: Syntax.RangeForStatement -> Ast.Expr
rangeForStatementToExpr rfs =

      let typ = Syntax.rangeForStatementType rfs
          var = Syntax.rangeForStatementVariable rfs
          range = Syntax.rangeForStatementRange rfs
          body = Syntax.rangeForStatementBody rfs
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "for",
          (Serialization.parens (Serialization.spaceSep [
            typeExpressionToExpr typ,
            (Serialization.cst var),
            (Serialization.cst ":"),
            (expressionToExpr range)]))],
        (statementToExpr body)])
-- | Convert a relational expression to an expression
relationalExpressionToExpr :: Syntax.RelationalExpression -> Ast.Expr
relationalExpressionToExpr e =
    case e of
      Syntax.RelationalExpressionShift v0 -> shiftExpressionToExpr v0
      Syntax.RelationalExpressionLess v0 -> lessOperationToExpr v0
      Syntax.RelationalExpressionGreater v0 -> greaterOperationToExpr v0
      Syntax.RelationalExpressionLessEqual v0 -> lessEqualOperationToExpr v0
      Syntax.RelationalExpressionGreaterEqual v0 -> greaterEqualOperationToExpr v0
-- | Convert a right shift operation to an expression
rightShiftOperationToExpr :: Syntax.RightShiftOperation -> Ast.Expr
rightShiftOperationToExpr op =

      let left = Syntax.rightShiftOperationLeft op
          right = Syntax.rightShiftOperationRight op
      in (Serialization.spaceSep [
        shiftExpressionToExpr left,
        (Serialization.cst ">>"),
        (additiveExpressionToExpr right)])
-- | Convert a selection statement to an expression
selectionStatementToExpr :: Syntax.SelectionStatement -> Ast.Expr
selectionStatementToExpr ss =

      let cond = Syntax.selectionStatementCondition ss
          thenBranch = Syntax.selectionStatementThenBranch ss
          elseBranch = Syntax.selectionStatementElseBranch ss
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "if",
          (Serialization.parens (expressionToExpr cond))],
        (statementToExpr thenBranch),
        (Optionals.cases elseBranch (Serialization.cst "") (\stmt -> Serialization.newlineSep [
          Serialization.cst "else",
          (statementToExpr stmt)]))])
-- | Convert a set to an expression
setToExpr :: Syntax.Set -> Ast.Expr
setToExpr s =

      let elemType = Syntax.setElementType s
          elems = Syntax.setElements s
      in (Serialization.spaceSep [
        Serialization.cst "std::set<",
        (typeExpressionToExpr elemType),
        (Serialization.cst ">"),
        (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map expressionToExpr elems))])
-- | Convert a shift expression to an expression
shiftExpressionToExpr :: Syntax.ShiftExpression -> Ast.Expr
shiftExpressionToExpr e =
    case e of
      Syntax.ShiftExpressionAdditive v0 -> additiveExpressionToExpr v0
      Syntax.ShiftExpressionLeftShift v0 -> leftShiftOperationToExpr v0
      Syntax.ShiftExpressionRightShift v0 -> rightShiftOperationToExpr v0
-- | Convert a sizeof expression to an expression
sizeofExpressionToExpr :: Syntax.SizeofExpression -> Ast.Expr
sizeofExpressionToExpr se =
    Serialization.spaceSep [
      Serialization.cst "sizeof",
      (Serialization.parens (typeExpressionToExpr (Syntax.unSizeofExpression se)))]
-- | Convert a statement to an expression
statementToExpr :: Syntax.Statement -> Ast.Expr
statementToExpr s =
    case s of
      Syntax.StatementLabeled v0 -> labeledStatementToExpr v0
      Syntax.StatementCompound v0 -> compoundStatementToExpr v0
      Syntax.StatementSelection v0 -> selectionStatementToExpr v0
      Syntax.StatementSwitch v0 -> switchStatementToExpr v0
      Syntax.StatementIteration v0 -> iterationStatementToExpr v0
      Syntax.StatementJump v0 -> jumpStatementToExpr v0
      Syntax.StatementDeclaration v0 -> Serialization.withSemi (variableDeclarationToExpr False v0)
      Syntax.StatementExpression v0 -> Serialization.withSemi (expressionToExpr v0)
-- | Convert a subscript operation to an expression
subscriptOperationToExpr :: Syntax.SubscriptOperation -> Ast.Expr
subscriptOperationToExpr so =

      let array = Syntax.subscriptOperationArray so
          index = Syntax.subscriptOperationIndex so
      in (Serialization.noSep [
        postfixExpressionToExpr array,
        (Serialization.cst "["),
        (expressionToExpr index),
        (Serialization.cst "]")])
-- | Convert a subtract operation to an expression
subtractOperationToExpr :: Syntax.SubtractOperation -> Ast.Expr
subtractOperationToExpr op =

      let left = Syntax.subtractOperationLeft op
          right = Syntax.subtractOperationRight op
      in (Serialization.spaceSep [
        additiveExpressionToExpr left,
        (Serialization.cst "-"),
        (multiplicativeExpressionToExpr right)])
-- | Convert a switch statement to an expression
switchStatementToExpr :: Syntax.SwitchStatement -> Ast.Expr
switchStatementToExpr ss =

      let value = Syntax.switchStatementValue ss
          cases = Syntax.switchStatementCases ss
      in (Serialization.spaceSep [
        Serialization.cst "switch",
        (Serialization.parens (expressionToExpr value)),
        (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map caseStatementToExpr cases)))])
-- | Convert a template argument to an expression
templateArgumentToExpr :: Syntax.TemplateArgument -> Ast.Expr
templateArgumentToExpr a =
    case a of
      Syntax.TemplateArgumentType v0 -> typeExpressionToExpr v0
      Syntax.TemplateArgumentValue v0 -> expressionToExpr v0
-- | Convert a template declaration to an expression
templateDeclarationToExpr :: Syntax.TemplateDeclaration -> Ast.Expr
templateDeclarationToExpr td =

      let inline = Syntax.templateDeclarationInline td
          params = Syntax.templateDeclarationParameters td
          declaration = Syntax.templateDeclarationDeclaration td
          sep = Logic.ifElse inline Serialization.spaceSep Serialization.newlineSep
      in (sep [
        Serialization.noSep [
          Serialization.cst "template",
          (Serialization.angleBracesList Serialization.inlineStyle (Lists.map (\p -> Serialization.cst p) params))],
        (declarationToExpr declaration)])
-- | Convert a template function call operation to an expression
templateFunctionCallOperationToExpr :: Syntax.TemplateFunctionCallOperation -> Ast.Expr
templateFunctionCallOperationToExpr tfco =

      let func = Syntax.templateFunctionCallOperationFunction tfco
          templateArgs = Syntax.templateFunctionCallOperationTemplateArguments tfco
          args = Syntax.templateFunctionCallOperationArguments tfco
      in (Serialization.noSep [
        postfixExpressionToExpr func,
        (Serialization.angleBracesList Serialization.inlineStyle (Lists.map templateArgumentToExpr templateArgs)),
        (Serialization.parenListAdaptive (Lists.map expressionToExpr args))])
-- | Convert a template type to an expression
templateTypeToExpr :: Syntax.TemplateType -> Ast.Expr
templateTypeToExpr tt =

      let name = Syntax.templateTypeName tt
          args = Syntax.templateTypeArguments tt
      in (Serialization.noSep [
        Serialization.cst name,
        (Serialization.angleBracesList Serialization.inlineStyle (Lists.map templateArgumentToExpr args))])
-- | Convert a ternary expression to an expression
ternaryExpressionToExpr :: Syntax.TernaryExpression -> Ast.Expr
ternaryExpressionToExpr te =

      let cond = Syntax.ternaryExpressionCondition te
          trueExpr = Syntax.ternaryExpressionTrueExpr te
          falseExpr = Syntax.ternaryExpressionFalseExpr te
      in (Serialization.spaceSep [
        logicalOrExpressionToExpr cond,
        (Serialization.cst "?"),
        (expressionToExpr trueExpr),
        (Serialization.cst ":"),
        (conditionalExpressionToExpr falseExpr)])
-- | Convert a string to a C++ comment. Empty single-line comments emit `//` (no trailing space).
toCppComments :: String -> Bool -> String
toCppComments s isMultiline =
    Logic.ifElse isMultiline (Strings.cat [
      "/* ",
      s,
      " */"]) (Logic.ifElse (Equality.equal s "") "//" (Strings.cat2 "// " s))
-- | Convert a type expression to an expression
typeExpressionToExpr :: Syntax.TypeExpression -> Ast.Expr
typeExpressionToExpr t =
    case t of
      Syntax.TypeExpressionBasic v0 -> basicTypeToExpr v0
      Syntax.TypeExpressionQualified v0 -> qualifiedTypeToExpr v0
      Syntax.TypeExpressionTemplate v0 -> templateTypeToExpr v0
      Syntax.TypeExpressionFunction v0 -> functionTypeToExpr v0
      Syntax.TypeExpressionAuto -> Serialization.cst "auto"
-- | Convert a typedef declaration to an expression
typedefDeclarationToExpr :: Syntax.TypedefDeclaration -> Ast.Expr
typedefDeclarationToExpr td =

      let name = Syntax.typedefDeclarationName td
          typ = Syntax.typedefDeclarationType td
          isUsing = Syntax.typedefDeclarationIsUsing td
      in (Logic.ifElse isUsing (Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "using " name),
        (Serialization.cst "="),
        (typeExpressionToExpr typ)])) (Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "typedef",
        (typeExpressionToExpr typ),
        (Serialization.cst name)])))
-- | Convert a unary expression to an expression
unaryExpressionToExpr :: Syntax.UnaryExpression -> Ast.Expr
unaryExpressionToExpr e =
    case e of
      Syntax.UnaryExpressionPostfix v0 -> postfixExpressionToExpr v0
      Syntax.UnaryExpressionUnaryOp v0 -> unaryOperationToExpr v0
      Syntax.UnaryExpressionSizeof v0 -> sizeofExpressionToExpr v0
-- | Convert a unary operation to an expression
unaryOperationToExpr :: Syntax.UnaryOperation -> Ast.Expr
unaryOperationToExpr uo =

      let op = Syntax.unaryOperationOperator uo
          operand = Syntax.unaryOperationOperand uo
      in (Serialization.spaceSep [
        unaryOperatorToExpr op,
        (unaryExpressionToExpr operand)])
-- | Convert a unary operator to an expression
unaryOperatorToExpr :: Syntax.UnaryOperator -> Ast.Expr
unaryOperatorToExpr op =
    case op of
      Syntax.UnaryOperatorPlus -> Serialization.cst "+"
      Syntax.UnaryOperatorMinus -> Serialization.cst "-"
      Syntax.UnaryOperatorLogicalNot -> Serialization.cst "!"
      Syntax.UnaryOperatorBitwiseNot -> Serialization.cst "~"
      Syntax.UnaryOperatorDereference -> Serialization.cst "*"
      Syntax.UnaryOperatorAddressOf -> Serialization.cst "&"
      Syntax.UnaryOperatorPreIncrement -> Serialization.cst "++"
      Syntax.UnaryOperatorPreDecrement -> Serialization.cst "--"
-- | Convert an undef directive to an expression
undefDirectiveToExpr :: Syntax.UndefDirective -> Ast.Expr
undefDirectiveToExpr ud =
    Serialization.spaceSep [
      Serialization.cst "#undef",
      (Serialization.cst (Syntax.undefDirectiveName ud))]
-- | Convert a variable declaration to an expression
variableDeclarationToExpr :: Bool -> Syntax.VariableDeclaration -> Ast.Expr
variableDeclarationToExpr commas vd =

      let typ = Syntax.variableDeclarationType vd
          name = Syntax.variableDeclarationName vd
          init = Syntax.variableDeclarationInitializer vd
          isAuto = Syntax.variableDeclarationIsAuto vd
          terminator = Logic.ifElse commas Serialization.withComma Serialization.withSemi
      in (terminator (Serialization.spaceSep (Lists.concat [
        Logic.ifElse isAuto [
          Serialization.cst "auto"] (Optionals.cases typ [] (\t -> [
          typeExpressionToExpr t])),
        [
          Serialization.cst name],
        (Optionals.cases init [] (\expr -> [
          Serialization.cst "=",
          (expressionToExpr expr)]))])))
-- | Convert a vector to an expression
vectorToExpr :: Syntax.Vector -> Ast.Expr
vectorToExpr v =

      let elemType = Syntax.vectorElementType v
          elems = Syntax.vectorElements v
      in (Serialization.spaceSep [
        Serialization.cst "std::vector<",
        (typeExpressionToExpr elemType),
        (Serialization.cst ">"),
        (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map expressionToExpr elems))])
-- | Convert a visitor to an expression
visitorToExpr :: Syntax.Visitor -> Ast.Expr
visitorToExpr v =
    case v of
      Syntax.VisitorLambda v0 -> lambdaExpressionToExpr v0
      Syntax.VisitorOverloaded v0 -> overloadedLambdasToExpr v0
-- | Convert a warning directive to an expression
warningDirectiveToExpr :: Syntax.WarningDirective -> Ast.Expr
warningDirectiveToExpr wd =
    Serialization.spaceSep [
      Serialization.cst "#warning",
      (Serialization.cst (Syntax.warningDirectiveMessage wd))]
-- | Convert a while statement to an expression
whileStatementToExpr :: Syntax.WhileStatement -> Ast.Expr
whileStatementToExpr ws =

      let cond = Syntax.whileStatementCondition ws
          body = Syntax.whileStatementBody ws
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "while",
          (Serialization.parens (expressionToExpr cond))],
        (statementToExpr body)])
