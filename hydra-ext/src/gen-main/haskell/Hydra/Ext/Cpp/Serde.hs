-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting C++ AST to abstract expressions

module Hydra.Ext.Cpp.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Cpp.Syntax as Syntax
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Convert an access specifier to an expression
encodeAccessSpecifier :: Syntax.AccessSpecifier -> Ast.Expr
encodeAccessSpecifier a =
    case a of
      Syntax.AccessSpecifierPublic -> Serialization.cst "public"
      Syntax.AccessSpecifierProtected -> Serialization.cst "protected"
      Syntax.AccessSpecifierPrivate -> Serialization.cst "private"
      Syntax.AccessSpecifierNone -> Serialization.cst ""

-- | Convert an add operation to an expression
encodeAddOperation :: Syntax.AddOperation -> Ast.Expr
encodeAddOperation op =

      let left = Syntax.addOperationLeft op
          right = Syntax.addOperationRight op
      in (Serialization.spaceSep [
        encodeAdditiveExpression left,
        (Serialization.cst "+"),
        (encodeMultiplicativeExpression right)])

-- | Convert an additive expression to an expression
encodeAdditiveExpression :: Syntax.AdditiveExpression -> Ast.Expr
encodeAdditiveExpression e =
    case e of
      Syntax.AdditiveExpressionMultiplicative v0 -> encodeMultiplicativeExpression v0
      Syntax.AdditiveExpressionAdd v0 -> encodeAddOperation v0
      Syntax.AdditiveExpressionSubtract v0 -> encodeSubtractOperation v0

-- | Convert an and expression to an expression
encodeAndExpression :: Syntax.AndExpression -> Ast.Expr
encodeAndExpression e =
    case e of
      Syntax.AndExpressionEquality v0 -> encodeEqualityExpression v0
      Syntax.AndExpressionBitwiseAnd v0 -> encodeBitwiseAndOperation v0

-- | Convert an assignment expression to an expression
encodeAssignmentExpression :: Syntax.AssignmentExpression -> Ast.Expr
encodeAssignmentExpression a =
    case a of
      Syntax.AssignmentExpressionConditional v0 -> encodeConditionalExpression v0
      Syntax.AssignmentExpressionAssignment v0 -> encodeExplicitAssignment v0

-- | Convert an assignment operator to an expression
encodeAssignmentOperator :: Syntax.AssignmentOperator -> Ast.Expr
encodeAssignmentOperator op =
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
encodeBaseSpecifier :: Syntax.BaseSpecifier -> Ast.Expr
encodeBaseSpecifier bs =

      let access = Syntax.baseSpecifierAccess bs
          name = Syntax.baseSpecifierName bs
      in (Serialization.spaceSep [
        encodeAccessSpecifier access,
        (Serialization.cst name)])

-- | Convert a basic type to an expression
encodeBasicType :: Syntax.BasicType -> Ast.Expr
encodeBasicType t =
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
encodeBitwiseAndOperation :: Syntax.BitwiseAndOperation -> Ast.Expr
encodeBitwiseAndOperation op =

      let left = Syntax.bitwiseAndOperationLeft op
          right = Syntax.bitwiseAndOperationRight op
      in (Serialization.spaceSep [
        encodeAndExpression left,
        (Serialization.cst "&"),
        (encodeEqualityExpression right)])

-- | Convert a bitwise or operation to an expression
encodeBitwiseOrOperation :: Syntax.BitwiseOrOperation -> Ast.Expr
encodeBitwiseOrOperation op =

      let left = Syntax.bitwiseOrOperationLeft op
          right = Syntax.bitwiseOrOperationRight op
      in (Serialization.spaceSep [
        encodeInclusiveOrExpression left,
        (Serialization.cst "|"),
        (encodeExclusiveOrExpression right)])

-- | Convert a bitwise xor operation to an expression
encodeBitwiseXorOperation :: Syntax.BitwiseXorOperation -> Ast.Expr
encodeBitwiseXorOperation op =

      let left = Syntax.bitwiseXorOperationLeft op
          right = Syntax.bitwiseXorOperationRight op
      in (Serialization.spaceSep [
        encodeExclusiveOrExpression left,
        (Serialization.cst "^"),
        (encodeAndExpression right)])

-- | Convert a boolean literal to an expression
encodeBooleanLiteral :: Syntax.BooleanLiteral -> Ast.Expr
encodeBooleanLiteral bl = Logic.ifElse (Syntax.unBooleanLiteral bl) (Serialization.cst "true") (Serialization.cst "false")

-- | Convert a capture to an expression
encodeCapture :: Syntax.Capture -> Ast.Expr
encodeCapture cap =

      let name = Syntax.captureName cap
          byRef = Syntax.captureByReference cap
      in (Logic.ifElse byRef (Serialization.cst (Strings.cat2 "&" name)) (Serialization.cst name))

-- | Convert a capture list to an expression
encodeCaptureList :: Syntax.CaptureList -> Ast.Expr
encodeCaptureList cl =
    case cl of
      Syntax.CaptureListCaptureByValue -> Serialization.cst "[=]"
      Syntax.CaptureListCaptures v0 -> Serialization.bracketList Serialization.inlineStyle (Lists.map encodeCapture v0)

-- | Convert a case statement to an expression
encodeCaseStatement :: Syntax.CaseStatement -> Ast.Expr
encodeCaseStatement stmt =
    case stmt of
      Syntax.CaseStatementCase v0 -> encodeCaseValue v0
      Syntax.CaseStatementDefault v0 -> Serialization.spaceSep [
        Serialization.cst "default:",
        (encodeStatement v0)]

-- | Convert a case value to an expression
encodeCaseValue :: Syntax.CaseValue -> Ast.Expr
encodeCaseValue cv =

      let value = Syntax.caseValueValue cv
          statement = Syntax.caseValueStatement cv
      in (Serialization.spaceSep [
        Serialization.cst "case",
        (Serialization.noSep [
          encodeExpression value,
          (Serialization.cst ":")]),
        (encodeStatement statement)])

-- | Convert a class body to an expression
encodeClassBody :: Bool -> Syntax.ClassBody -> Ast.Expr
encodeClassBody commas cb =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map (encodeMemberSpecification commas) (Syntax.unClassBody cb)))

-- | Convert a class declaration to an expression
encodeClassDeclaration :: Syntax.ClassDeclaration -> Ast.Expr
encodeClassDeclaration cd =

      let spec = Syntax.classDeclarationSpecifier cd
          mbody = Syntax.classDeclarationBody cd
          key = Syntax.classSpecifierKey spec
          isEnum = Logic.or (Equality.equal key Syntax.ClassKeyEnum) (Equality.equal key Syntax.ClassKeyEnumClass)
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Just (encodeClassSpecifier spec),
        (Maybes.map (\body -> encodeClassBody isEnum body) mbody)])))

-- | Convert a class key to an expression
encodeClassKey :: Syntax.ClassKey -> Ast.Expr
encodeClassKey k =
    case k of
      Syntax.ClassKeyClass -> Serialization.cst "class"
      Syntax.ClassKeyEnum -> Serialization.cst "enum"
      Syntax.ClassKeyEnumClass -> Serialization.cst "enum class"
      Syntax.ClassKeyStruct -> Serialization.cst "struct"

-- | Convert a class specifier to an expression
encodeClassSpecifier :: Syntax.ClassSpecifier -> Ast.Expr
encodeClassSpecifier cs =

      let key = Syntax.classSpecifierKey cs
          name = Syntax.classSpecifierName cs
          inheritance = Syntax.classSpecifierInheritance cs
      in (Serialization.spaceSep (Lists.concat [
        [
          encodeClassKey key,
          (Serialization.cst name)],
        (Logic.ifElse (Lists.null inheritance) [] [
          Serialization.cst ":",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeBaseSpecifier inheritance))])]))

-- | Convert a comma expression to an expression
encodeCommaExpression :: Syntax.CommaExpression -> Ast.Expr
encodeCommaExpression ce =

      let left = Syntax.commaExpressionLeft ce
          right = Syntax.commaExpressionRight ce
      in (Serialization.spaceSep [
        encodeExpression left,
        (Serialization.cst ","),
        (encodeAssignmentExpression right)])

-- | Convert a comment to an expression
encodeComment :: Syntax.Comment -> Ast.Expr
encodeComment c =

      let text = Syntax.commentText c
          isMultiline = Syntax.commentIsMultiline c
      in (Serialization.cst (toCppComments text isMultiline))

-- | Convert a compound statement to an expression
encodeCompoundStatement :: Syntax.CompoundStatement -> Ast.Expr
encodeCompoundStatement cs =
    Serialization.curlyBracesList (Just "") Serialization.fullBlockStyle (Lists.map encodeStatement (Syntax.unCompoundStatement cs))

-- | Convert a conditional expression to an expression
encodeConditionalExpression :: Syntax.ConditionalExpression -> Ast.Expr
encodeConditionalExpression c =
    case c of
      Syntax.ConditionalExpressionLogicalOr v0 -> encodeLogicalOrExpression v0
      Syntax.ConditionalExpressionTernary v0 -> encodeTernaryExpression v0

-- | Convert a constructor declaration to an expression
encodeConstructorDeclaration :: Syntax.ConstructorDeclaration -> Ast.Expr
encodeConstructorDeclaration cd =

      let name = Syntax.constructorDeclarationName cd
          params = Syntax.constructorDeclarationParameters cd
          inits = Syntax.constructorDeclarationInitializers cd
          body = Syntax.constructorDeclarationBody cd
      in (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.noSep [
          Serialization.cst name,
          (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeParameter params)))]),
        (Logic.ifElse (Lists.null inits) Nothing (Just (Serialization.spaceSep [
          Serialization.cst ":",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeMemInitializer inits))]))),
        (Just (encodeFunctionBody body))]))

-- | Convert a declaration to an expression
encodeDeclaration :: Syntax.Declaration -> Ast.Expr
encodeDeclaration d =
    case d of
      Syntax.DeclarationPreprocessor v0 -> encodePreprocessorDirective v0
      Syntax.DeclarationClass v0 -> encodeClassDeclaration v0
      Syntax.DeclarationFunction v0 -> encodeFunctionDeclaration v0
      Syntax.DeclarationVariable v0 -> encodeVariableDeclaration False v0
      Syntax.DeclarationTypedef v0 -> encodeTypedefDeclaration v0
      Syntax.DeclarationNamespace v0 -> encodeNamespaceDeclaration v0
      Syntax.DeclarationTemplate v0 -> encodeTemplateDeclaration v0

-- | Convert a define directive to an expression
encodeDefineDirective :: Syntax.DefineDirective -> Ast.Expr
encodeDefineDirective dd =

      let name = Syntax.defineDirectiveName dd
          params = Syntax.defineDirectiveParameters dd
          replacement = Syntax.defineDirectiveReplacement dd
      in (Serialization.spaceSep (Lists.concat [
        [
          Serialization.cst "#define",
          (Serialization.cst name)],
        (Maybes.maybe [] (\ps -> [
          Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map (\p -> Serialization.cst p) ps))]) params),
        (Maybes.maybe [] (\r -> [
          Serialization.cst r]) replacement)]))

-- | Convert a destructor declaration to an expression
encodeDestructorDeclaration :: Syntax.DestructorDeclaration -> Ast.Expr
encodeDestructorDeclaration dd =

      let prefixSpecs = Syntax.destructorDeclarationPrefixSpecifiers dd
          name = Syntax.destructorDeclarationName dd
          suffixSpecs = Syntax.destructorDeclarationSuffixSpecifiers dd
          body = Syntax.destructorDeclarationBody dd
      in (Serialization.spaceSep (Lists.concat [
        Lists.map encodeFunctionSpecifierPrefix prefixSpecs,
        [
          Serialization.noSep [
            Serialization.cst (Strings.cat2 "~" name),
            (Serialization.parens (Serialization.cst ""))]],
        (Lists.map encodeFunctionSpecifierSuffix suffixSpecs),
        [
          encodeFunctionBody body]]))

-- | Convert a divide operation to an expression
encodeDivideOperation :: Syntax.DivideOperation -> Ast.Expr
encodeDivideOperation op =

      let left = Syntax.divideOperationLeft op
          right = Syntax.divideOperationRight op
      in (Serialization.spaceSep [
        encodeMultiplicativeExpression left,
        (Serialization.cst "/"),
        (encodeUnaryExpression right)])

-- | Convert a do statement to an expression
encodeDoStatement :: Syntax.DoStatement -> Ast.Expr
encodeDoStatement ds =

      let body = Syntax.doStatementBody ds
          cond = Syntax.doStatementCondition ds
      in (Serialization.newlineSep [
        Serialization.cst "do",
        (encodeStatement body),
        (Serialization.withSemi (Serialization.spaceSep [
          Serialization.cst "while",
          (Serialization.parens (encodeExpression cond))]))])

-- | Convert an elif directive to an expression
encodeElifDirective :: Syntax.ElifDirective -> Ast.Expr
encodeElifDirective ed =
    Serialization.spaceSep [
      Serialization.cst "#elif",
      (Serialization.cst (Syntax.elifDirectiveCondition ed))]

-- | Convert an else directive to an expression
encodeElseDirective :: t0 -> Ast.Expr
encodeElseDirective ed = Serialization.cst "#else"

-- | Convert an endif directive to an expression
encodeEndifDirective :: t0 -> Ast.Expr
encodeEndifDirective ed = Serialization.cst "#endif"

-- | Convert an equal operation to an expression
encodeEqualOperation :: Syntax.EqualOperation -> Ast.Expr
encodeEqualOperation op =

      let left = Syntax.equalOperationLeft op
          right = Syntax.equalOperationRight op
      in (Serialization.spaceSep [
        encodeEqualityExpression left,
        (Serialization.cst "=="),
        (encodeRelationalExpression right)])

-- | Convert an equality expression to an expression
encodeEqualityExpression :: Syntax.EqualityExpression -> Ast.Expr
encodeEqualityExpression e =
    case e of
      Syntax.EqualityExpressionRelational v0 -> encodeRelationalExpression v0
      Syntax.EqualityExpressionEqual v0 -> encodeEqualOperation v0
      Syntax.EqualityExpressionNotEqual v0 -> encodeNotEqualOperation v0

-- | Convert an error directive to an expression
encodeErrorDirective :: Syntax.ErrorDirective -> Ast.Expr
encodeErrorDirective ed =
    Serialization.spaceSep [
      Serialization.cst "#error",
      (Serialization.cst (Syntax.errorDirectiveMessage ed))]

-- | Convert an exclusive or expression to an expression
encodeExclusiveOrExpression :: Syntax.ExclusiveOrExpression -> Ast.Expr
encodeExclusiveOrExpression e =
    case e of
      Syntax.ExclusiveOrExpressionAnd v0 -> encodeAndExpression v0
      Syntax.ExclusiveOrExpressionBitwiseXor v0 -> encodeBitwiseXorOperation v0

-- | Convert an explicit assignment to an expression
encodeExplicitAssignment :: Syntax.ExplicitAssignment -> Ast.Expr
encodeExplicitAssignment ea =

      let left = Syntax.explicitAssignmentLeft ea
          op = Syntax.explicitAssignmentOp ea
          right = Syntax.explicitAssignmentRight ea
      in (Serialization.spaceSep [
        encodeLogicalOrExpression left,
        (encodeAssignmentOperator op),
        (encodeAssignmentExpression right)])

-- | Convert an expression to an expression
encodeExpression :: Syntax.Expression -> Ast.Expr
encodeExpression e =
    case e of
      Syntax.ExpressionAssignment v0 -> encodeAssignmentExpression v0
      Syntax.ExpressionComma v0 -> encodeCommaExpression v0

-- | Convert a for-init to an expression
encodeForInit :: Syntax.ForInit -> Ast.Expr
encodeForInit i =
    case i of
      Syntax.ForInitExpression v0 -> encodeExpression v0
      Syntax.ForInitDeclaration v0 -> encodeVariableDeclaration False v0
      Syntax.ForInitEmpty -> Serialization.cst ""

-- | Convert a for statement to an expression
encodeForStatement :: Syntax.ForStatement -> Ast.Expr
encodeForStatement fs =

      let init = Syntax.forStatementInit fs
          cond = Syntax.forStatementCondition fs
          inc = Syntax.forStatementIncrement fs
          body = Syntax.forStatementBody fs
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "for",
          (Serialization.parens (Serialization.noSep [
            encodeForInit init,
            (Serialization.cst ";"),
            (encodeExpression cond),
            (Serialization.cst ";"),
            (encodeExpression inc)]))],
        (encodeStatement body)])

-- | Convert a function application to an expression
encodeFunctionApplication :: Syntax.FunctionApplication -> Ast.Expr
encodeFunctionApplication fa =

      let func = Syntax.functionApplicationFunction fa
          args = Syntax.functionApplicationArguments fa
      in (Serialization.spaceSep [
        encodeFunctionIdentifier func,
        (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeExpression args)))])

-- | Convert a function body to an expression
encodeFunctionBody :: Syntax.FunctionBody -> Ast.Expr
encodeFunctionBody b =
    case b of
      Syntax.FunctionBodyCompound v0 -> encodeCompoundStatement v0
      Syntax.FunctionBodyDeclaration -> Serialization.cst ";"
      Syntax.FunctionBodyPure -> Serialization.withSemi (Serialization.cst "= 0")
      Syntax.FunctionBodyDefault -> Serialization.withSemi (Serialization.cst "= default")

-- | Convert a function call operation to an expression
encodeFunctionCallOperation :: Syntax.FunctionCallOperation -> Ast.Expr
encodeFunctionCallOperation fco =

      let func = Syntax.functionCallOperationFunction fco
          args = Syntax.functionCallOperationArguments fco
      in (Serialization.noSep [
        encodePostfixExpression func,
        (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeExpression args)))])

-- | Convert a function declaration to an expression
encodeFunctionDeclaration :: Syntax.FunctionDeclaration -> Ast.Expr
encodeFunctionDeclaration fd =

      let prefixSpecs = Syntax.functionDeclarationPrefixSpecifiers fd
          retType = Syntax.functionDeclarationReturnType fd
          name = Syntax.functionDeclarationName fd
          params = Syntax.functionDeclarationParameters fd
          suffixSpecs = Syntax.functionDeclarationSuffixSpecifiers fd
          body = Syntax.functionDeclarationBody fd
      in (Serialization.spaceSep (Lists.concat [
        Lists.map encodeFunctionSpecifierPrefix prefixSpecs,
        [
          encodeTypeExpression retType,
          (Serialization.noSep [
            Serialization.cst name,
            (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeParameter params)))])],
        (Lists.map encodeFunctionSpecifierSuffix suffixSpecs),
        [
          encodeFunctionBody body]]))

-- | Convert a function identifier to an expression
encodeFunctionIdentifier :: Syntax.FunctionIdentifier -> Ast.Expr
encodeFunctionIdentifier f =
    case f of
      Syntax.FunctionIdentifierSimple v0 -> Serialization.cst v0
      Syntax.FunctionIdentifierQualified v0 -> encodeQualifiedIdentifier v0

-- | Convert a function specifier prefix to an expression
encodeFunctionSpecifierPrefix :: Syntax.FunctionSpecifierPrefix -> Ast.Expr
encodeFunctionSpecifierPrefix s =
    case s of
      Syntax.FunctionSpecifierPrefixInline -> Serialization.cst "inline"
      Syntax.FunctionSpecifierPrefixVirtual -> Serialization.cst "virtual"
      Syntax.FunctionSpecifierPrefixStatic -> Serialization.cst "static"
      Syntax.FunctionSpecifierPrefixExplicit -> Serialization.cst "explicit"

-- | Convert a function specifier suffix to an expression
encodeFunctionSpecifierSuffix :: Syntax.FunctionSpecifierSuffix -> Ast.Expr
encodeFunctionSpecifierSuffix s =
    case s of
      Syntax.FunctionSpecifierSuffixConst -> Serialization.cst "const"
      Syntax.FunctionSpecifierSuffixNoexcept -> Serialization.cst "noexcept"
      Syntax.FunctionSpecifierSuffixOverride -> Serialization.cst "override"
      Syntax.FunctionSpecifierSuffixFinal -> Serialization.cst "final"

-- | Convert a function type to an expression
encodeFunctionType :: Syntax.FunctionType -> Ast.Expr
encodeFunctionType ft =

      let retType = Syntax.functionTypeReturnType ft
          params = Syntax.functionTypeParameters ft
      in (Serialization.spaceSep [
        encodeTypeExpression retType,
        (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeParameter params)))])

-- | Convert a greater-than-or-equal operation to an expression
encodeGreaterEqualOperation :: Syntax.GreaterEqualOperation -> Ast.Expr
encodeGreaterEqualOperation op =

      let left = Syntax.greaterEqualOperationLeft op
          right = Syntax.greaterEqualOperationRight op
      in (Serialization.spaceSep [
        encodeRelationalExpression left,
        (Serialization.cst ">="),
        (encodeShiftExpression right)])

-- | Convert a greater-than operation to an expression
encodeGreaterOperation :: Syntax.GreaterOperation -> Ast.Expr
encodeGreaterOperation op =

      let left = Syntax.greaterOperationLeft op
          right = Syntax.greaterOperationRight op
      in (Serialization.spaceSep [
        encodeRelationalExpression left,
        (Serialization.cst ">"),
        (encodeShiftExpression right)])

-- | Convert an if directive to an expression
encodeIfDirective :: Syntax.IfDirective -> Ast.Expr
encodeIfDirective ifd =
    Serialization.spaceSep [
      Serialization.cst "#if",
      (Serialization.cst (Syntax.ifDirectiveCondition ifd))]

-- | Convert an ifdef directive to an expression
encodeIfdefDirective :: Syntax.IfdefDirective -> Ast.Expr
encodeIfdefDirective id =
    Serialization.spaceSep [
      Serialization.cst "#ifdef",
      (Serialization.cst (Syntax.ifdefDirectiveIdentifier id))]

-- | Convert an ifndef directive to an expression
encodeIfndefDirective :: Syntax.IfndefDirective -> Ast.Expr
encodeIfndefDirective ind =
    Serialization.spaceSep [
      Serialization.cst "#ifndef",
      (Serialization.cst (Syntax.ifndefDirectiveIdentifier ind))]

-- | Convert an include directive to an expression
encodeIncludeDirective :: Syntax.IncludeDirective -> Ast.Expr
encodeIncludeDirective incl =

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
encodeInclusiveOrExpression :: Syntax.InclusiveOrExpression -> Ast.Expr
encodeInclusiveOrExpression e =
    case e of
      Syntax.InclusiveOrExpressionExclusiveOr v0 -> encodeExclusiveOrExpression v0
      Syntax.InclusiveOrExpressionBitwiseOr v0 -> encodeBitwiseOrOperation v0

-- | Convert an integer literal to an expression
encodeIntegerLiteral :: Syntax.IntegerLiteral -> Ast.Expr
encodeIntegerLiteral i =
    case i of
      Syntax.IntegerLiteralDecimal v0 -> Serialization.cst (Literals.showBigint v0)
      Syntax.IntegerLiteralHexadecimal v0 -> Serialization.cst (Strings.cat2 "0x" v0)
      Syntax.IntegerLiteralOctal v0 -> Serialization.cst (Strings.cat2 "0" v0)
      Syntax.IntegerLiteralBinary v0 -> Serialization.cst (Strings.cat2 "0b" v0)

-- | Convert an iteration statement to an expression
encodeIterationStatement :: Syntax.IterationStatement -> Ast.Expr
encodeIterationStatement i =
    case i of
      Syntax.IterationStatementWhile v0 -> encodeWhileStatement v0
      Syntax.IterationStatementDo v0 -> encodeDoStatement v0
      Syntax.IterationStatementFor v0 -> encodeForStatement v0
      Syntax.IterationStatementRangeFor v0 -> encodeRangeForStatement v0

-- | Convert a jump statement to an expression
encodeJumpStatement :: Syntax.JumpStatement -> Ast.Expr
encodeJumpStatement j =
    case j of
      Syntax.JumpStatementBreak -> Serialization.withSemi (Serialization.cst "break")
      Syntax.JumpStatementContinue -> Serialization.withSemi (Serialization.cst "continue")
      Syntax.JumpStatementReturnValue v0 -> Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "return",
        (encodeExpression v0)])
      Syntax.JumpStatementReturnVoid -> Serialization.withSemi (Serialization.cst "return")
      Syntax.JumpStatementThrow v0 -> Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "throw",
        (encodeExpression v0)])

-- | Convert a labeled statement to an expression
encodeLabeledStatement :: Syntax.LabeledStatement -> Ast.Expr
encodeLabeledStatement ls =

      let label = Syntax.labeledStatementLabel ls
          stmt = Syntax.labeledStatementStatement ls
      in (Serialization.newlineSep [
        Serialization.cst (Strings.cat2 label ":"),
        (encodeStatement stmt)])

-- | Convert a lambda expression to an expression
encodeLambdaExpression :: Syntax.LambdaExpression -> Ast.Expr
encodeLambdaExpression le =

      let captures = Syntax.lambdaExpressionCaptures le
          params = Syntax.lambdaExpressionParameters le
          retType = Syntax.lambdaExpressionReturnType le
          body = Syntax.lambdaExpressionBody le
      in (Serialization.spaceSep [
        encodeCaptureList captures,
        (Logic.ifElse (Lists.null params) (Serialization.parens (Serialization.cst "")) (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeParameter params)))),
        (Maybes.maybe (Serialization.cst "") (\t -> Serialization.spaceSep [
          Serialization.cst "->",
          (encodeTypeExpression t)]) retType),
        (encodeCompoundStatement body)])

-- | Convert a left shift operation to an expression
encodeLeftShiftOperation :: Syntax.LeftShiftOperation -> Ast.Expr
encodeLeftShiftOperation op =

      let left = Syntax.leftShiftOperationLeft op
          right = Syntax.leftShiftOperationRight op
      in (Serialization.spaceSep [
        encodeShiftExpression left,
        (Serialization.cst "<<"),
        (encodeAdditiveExpression right)])

-- | Convert a less-than-or-equal operation to an expression
encodeLessEqualOperation :: Syntax.LessEqualOperation -> Ast.Expr
encodeLessEqualOperation op =

      let left = Syntax.lessEqualOperationLeft op
          right = Syntax.lessEqualOperationRight op
      in (Serialization.spaceSep [
        encodeRelationalExpression left,
        (Serialization.cst "<="),
        (encodeShiftExpression right)])

-- | Convert a less-than operation to an expression
encodeLessOperation :: Syntax.LessOperation -> Ast.Expr
encodeLessOperation op =

      let left = Syntax.lessOperationLeft op
          right = Syntax.lessOperationRight op
      in (Serialization.spaceSep [
        encodeRelationalExpression left,
        (Serialization.cst "<"),
        (encodeShiftExpression right)])

-- | Convert a line directive to an expression
encodeLineDirective :: Syntax.LineDirective -> Ast.Expr
encodeLineDirective ld =

      let lineNumber = Syntax.lineDirectiveLineNumber ld
          filename = Syntax.lineDirectiveFilename ld
      in (Serialization.spaceSep (Lists.concat [
        [
          Serialization.cst "#line",
          (Serialization.cst (Literals.showInt32 lineNumber))],
        (Maybes.maybe [] (\f -> [
          Serialization.cst (Strings.cat [
            "\"",
            f,
            "\""])]) filename)]))

-- | Convert a literal to an expression
encodeLiteral :: Syntax.Literal -> Ast.Expr
encodeLiteral l =
    case l of
      Syntax.LiteralInteger v0 -> encodeIntegerLiteral v0
      Syntax.LiteralFloating v0 -> Serialization.cst (Literals.showBigfloat (Syntax.unFloatingLiteral v0))
      Syntax.LiteralCharacter v0 -> Serialization.cst (Strings.cat [
        "'",
        (Syntax.unCharacterLiteral v0),
        "'"])
      Syntax.LiteralString v0 -> Serialization.cst (Strings.cat [
        "\"",
        (Syntax.unStringLiteral v0),
        "\""])
      Syntax.LiteralBoolean v0 -> encodeBooleanLiteral v0
      Syntax.LiteralNull -> Serialization.cst "nullptr"

-- | Convert a logical and expression to an expression
encodeLogicalAndExpression :: Syntax.LogicalAndExpression -> Ast.Expr
encodeLogicalAndExpression e =
    case e of
      Syntax.LogicalAndExpressionInclusiveOr v0 -> encodeInclusiveOrExpression v0
      Syntax.LogicalAndExpressionLogicalAnd v0 -> encodeLogicalAndOperation v0

-- | Convert a logical and operation to an expression
encodeLogicalAndOperation :: Syntax.LogicalAndOperation -> Ast.Expr
encodeLogicalAndOperation op =

      let left = Syntax.logicalAndOperationLeft op
          right = Syntax.logicalAndOperationRight op
      in (Serialization.spaceSep [
        encodeLogicalAndExpression left,
        (Serialization.cst "&&"),
        (encodeInclusiveOrExpression right)])

-- | Convert a logical or expression to an expression
encodeLogicalOrExpression :: Syntax.LogicalOrExpression -> Ast.Expr
encodeLogicalOrExpression e =
    case e of
      Syntax.LogicalOrExpressionLogicalAnd v0 -> encodeLogicalAndExpression v0
      Syntax.LogicalOrExpressionLogicalOr v0 -> encodeLogicalOrOperation v0

-- | Convert a logical or operation to an expression
encodeLogicalOrOperation :: Syntax.LogicalOrOperation -> Ast.Expr
encodeLogicalOrOperation op =

      let left = Syntax.logicalOrOperationLeft op
          right = Syntax.logicalOrOperationRight op
      in (Serialization.spaceSep [
        encodeLogicalOrExpression left,
        (Serialization.cst "||"),
        (encodeLogicalAndExpression right)])

-- | Convert a map to an expression
encodeMap :: Syntax.Map -> Ast.Expr
encodeMap m =

      let keyType = Syntax.mapKeyType m
          valType = Syntax.mapValueType m
          entries = Syntax.mapEntries m
      in (Serialization.spaceSep [
        Serialization.cst "std::map<",
        (Serialization.commaSep Serialization.inlineStyle [
          encodeTypeExpression keyType,
          (encodeTypeExpression valType)]),
        (Serialization.cst ">"),
        (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map encodeMapEntry entries))])

-- | Convert a map entry to an expression
encodeMapEntry :: Syntax.MapEntry -> Ast.Expr
encodeMapEntry me =

      let key = Syntax.mapEntryKey me
          val = Syntax.mapEntryValue me
      in (Serialization.spaceSep [
        Serialization.curlyBracesList Nothing Serialization.inlineStyle [
          encodeExpression key],
        (Serialization.cst "->"),
        (encodeExpression val)])

-- | Convert a member initializer to an expression
encodeMemInitializer :: Syntax.MemInitializer -> Ast.Expr
encodeMemInitializer mi =

      let name = Syntax.memInitializerName mi
          args = Syntax.memInitializerArguments mi
      in (Serialization.noSep [
        Serialization.cst name,
        (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeExpression args)))])

-- | Convert a member access operation to an expression
encodeMemberAccessOperation :: Syntax.MemberAccessOperation -> Ast.Expr
encodeMemberAccessOperation mao =

      let obj = Syntax.memberAccessOperationObject mao
          member = Syntax.memberAccessOperationMember mao
      in (Serialization.noSep [
        encodePostfixExpression obj,
        (Serialization.cst "."),
        (Serialization.cst member)])

-- | Convert a member declaration to an expression
encodeMemberDeclaration :: Bool -> Syntax.MemberDeclaration -> Ast.Expr
encodeMemberDeclaration commas m =
    case m of
      Syntax.MemberDeclarationFunction v0 -> encodeFunctionDeclaration v0
      Syntax.MemberDeclarationVariable v0 -> encodeVariableDeclaration commas v0
      Syntax.MemberDeclarationConstructor v0 -> encodeConstructorDeclaration v0
      Syntax.MemberDeclarationDestructor v0 -> encodeDestructorDeclaration v0
      Syntax.MemberDeclarationNestedClass v0 -> encodeClassDeclaration v0
      Syntax.MemberDeclarationTemplate v0 -> encodeTemplateDeclaration v0

-- | Convert a member specification to an expression
encodeMemberSpecification :: Bool -> Syntax.MemberSpecification -> Ast.Expr
encodeMemberSpecification commas m =
    case m of
      Syntax.MemberSpecificationAccessLabel v0 -> Serialization.noSep [
        encodeAccessSpecifier v0,
        (Serialization.cst ":")]
      Syntax.MemberSpecificationMember v0 -> encodeMemberDeclaration commas v0

-- | Convert a modulo operation to an expression
encodeModuloOperation :: Syntax.ModuloOperation -> Ast.Expr
encodeModuloOperation op =

      let left = Syntax.moduloOperationLeft op
          right = Syntax.moduloOperationRight op
      in (Serialization.spaceSep [
        encodeMultiplicativeExpression left,
        (Serialization.cst "%"),
        (encodeUnaryExpression right)])

-- | Convert a multiplicative expression to an expression
encodeMultiplicativeExpression :: Syntax.MultiplicativeExpression -> Ast.Expr
encodeMultiplicativeExpression e =
    case e of
      Syntax.MultiplicativeExpressionUnary v0 -> encodeUnaryExpression v0
      Syntax.MultiplicativeExpressionMultiply v0 -> encodeMultiplyOperation v0
      Syntax.MultiplicativeExpressionDivide v0 -> encodeDivideOperation v0
      Syntax.MultiplicativeExpressionModulo v0 -> encodeModuloOperation v0

-- | Convert a multiply operation to an expression
encodeMultiplyOperation :: Syntax.MultiplyOperation -> Ast.Expr
encodeMultiplyOperation op =

      let left = Syntax.multiplyOperationLeft op
          right = Syntax.multiplyOperationRight op
      in (Serialization.spaceSep [
        encodeMultiplicativeExpression left,
        (Serialization.cst "*"),
        (encodeUnaryExpression right)])

-- | Convert a namespace declaration to an expression
encodeNamespaceDeclaration :: Syntax.NamespaceDeclaration -> Ast.Expr
encodeNamespaceDeclaration nd =

      let name = Syntax.namespaceDeclarationName nd
          decls = Syntax.namespaceDeclarationDeclarations nd
      in (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "namespace " name),
        (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map encodeDeclaration decls)))])

-- | Convert a not-equal operation to an expression
encodeNotEqualOperation :: Syntax.NotEqualOperation -> Ast.Expr
encodeNotEqualOperation op =

      let left = Syntax.notEqualOperationLeft op
          right = Syntax.notEqualOperationRight op
      in (Serialization.spaceSep [
        encodeEqualityExpression left,
        (Serialization.cst "!="),
        (encodeRelationalExpression right)])

-- | Convert an optional to an expression
encodeOptional :: Syntax.Optional -> Ast.Expr
encodeOptional opt =

      let valType = Syntax.optionalValueType opt
          val = Syntax.optionalValue opt
      in (Serialization.spaceSep [
        Serialization.cst "std::optional<",
        (encodeTypeExpression valType),
        (Serialization.cst ">"),
        (Maybes.maybe (Serialization.cst "{}") (\v -> Serialization.curlyBracesList Nothing Serialization.inlineStyle [
          encodeExpression v]) val)])

-- | Convert overloaded lambdas to an expression
encodeOverloadedLambdas :: Syntax.OverloadedLambdas -> Ast.Expr
encodeOverloadedLambdas ol =
    Serialization.spaceSep [
      Serialization.cst "overloaded",
      (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map encodeLambdaExpression (Syntax.unOverloadedLambdas ol))))]

-- | Convert a parameter to an expression
encodeParameter :: Syntax.Parameter -> Ast.Expr
encodeParameter p =

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
          encodeTypeExpression typ,
          nameExpr],
        (Maybes.maybe [] (\expr -> [
          Serialization.cst "=",
          (encodeExpression expr)]) defaultVal)]))

-- | Convert a pattern match to an expression
encodePatternMatch :: Syntax.PatternMatch -> Ast.Expr
encodePatternMatch pm =

      let visitor = Syntax.patternMatchVisitor pm
          variant = Syntax.patternMatchVariant pm
      in (Serialization.spaceSep [
        Serialization.cst "std::visit",
        (Serialization.parens (Serialization.commaSep Serialization.inlineStyle [
          encodeVisitor visitor,
          (encodeExpression variant)]))])

-- | Convert a pointer member access operation to an expression
encodePointerMemberAccessOperation :: Syntax.PointerMemberAccessOperation -> Ast.Expr
encodePointerMemberAccessOperation pmao =

      let ptr = Syntax.pointerMemberAccessOperationPointer pmao
          member = Syntax.pointerMemberAccessOperationMember pmao
      in (Serialization.noSep [
        encodePostfixExpression ptr,
        (Serialization.cst "->"),
        (Serialization.cst member)])

-- | Convert a postfix expression to an expression
encodePostfixExpression :: Syntax.PostfixExpression -> Ast.Expr
encodePostfixExpression e =
    case e of
      Syntax.PostfixExpressionPrimary v0 -> encodePrimaryExpression v0
      Syntax.PostfixExpressionSubscript v0 -> encodeSubscriptOperation v0
      Syntax.PostfixExpressionFunctionCall v0 -> encodeFunctionCallOperation v0
      Syntax.PostfixExpressionTemplateFunctionCall v0 -> encodeTemplateFunctionCallOperation v0
      Syntax.PostfixExpressionMemberAccess v0 -> encodeMemberAccessOperation v0
      Syntax.PostfixExpressionPointerMemberAccess v0 -> encodePointerMemberAccessOperation v0
      Syntax.PostfixExpressionPostIncrement v0 -> Serialization.noSep [
        encodePostfixExpression v0,
        (Serialization.cst "++")]
      Syntax.PostfixExpressionPostDecrement v0 -> Serialization.noSep [
        encodePostfixExpression v0,
        (Serialization.cst "--")]

-- | Convert a pragma directive to an expression
encodePragmaDirective :: Syntax.PragmaDirective -> Ast.Expr
encodePragmaDirective pd = Serialization.cst (Strings.cat2 "#pragma " (Syntax.pragmaDirectiveContent pd))

-- | Convert a preprocessor directive to an expression
encodePreprocessorDirective :: Syntax.PreprocessorDirective -> Ast.Expr
encodePreprocessorDirective d =
    case d of
      Syntax.PreprocessorDirectiveInclude v0 -> encodeIncludeDirective v0
      Syntax.PreprocessorDirectivePragma v0 -> encodePragmaDirective v0
      Syntax.PreprocessorDirectiveDefine v0 -> encodeDefineDirective v0
      Syntax.PreprocessorDirectiveUndef v0 -> encodeUndefDirective v0
      Syntax.PreprocessorDirectiveIfdef v0 -> encodeIfdefDirective v0
      Syntax.PreprocessorDirectiveIfndef v0 -> encodeIfndefDirective v0
      Syntax.PreprocessorDirectiveIf v0 -> encodeIfDirective v0
      Syntax.PreprocessorDirectiveElif v0 -> encodeElifDirective v0
      Syntax.PreprocessorDirectiveElse v0 -> encodeElseDirective v0
      Syntax.PreprocessorDirectiveEndif v0 -> encodeEndifDirective v0
      Syntax.PreprocessorDirectiveLine v0 -> encodeLineDirective v0
      Syntax.PreprocessorDirectiveError v0 -> encodeErrorDirective v0
      Syntax.PreprocessorDirectiveWarning v0 -> encodeWarningDirective v0

-- | Convert a primary expression to an expression
encodePrimaryExpression :: Syntax.PrimaryExpression -> Ast.Expr
encodePrimaryExpression e =
    case e of
      Syntax.PrimaryExpressionIdentifier v0 -> Serialization.cst v0
      Syntax.PrimaryExpressionLiteral v0 -> encodeLiteral v0
      Syntax.PrimaryExpressionParenthesized v0 -> Serialization.parens (encodeExpression v0)
      Syntax.PrimaryExpressionLambda v0 -> encodeLambdaExpression v0

-- | Convert a program to an expression
encodeProgram :: Syntax.Program -> Ast.Expr
encodeProgram prog =

      let preps = Syntax.programPreprocessorDirectives prog
          includes = Syntax.programIncludes prog
          decls = Syntax.programDeclarations prog
          separate = \sep -> \defs -> Logic.ifElse (Lists.null defs) Nothing (Just (sep defs))
      in (Serialization.doubleNewlineSep (Maybes.cat [
        separate Serialization.newlineSep (Lists.map encodePreprocessorDirective preps),
        (separate Serialization.newlineSep (Lists.map encodeIncludeDirective includes)),
        (separate Serialization.doubleNewlineSep (Lists.map encodeDeclaration decls))]))

-- | Convert a qualified identifier to an expression
encodeQualifiedIdentifier :: Syntax.QualifiedIdentifier -> Ast.Expr
encodeQualifiedIdentifier qi =

      let ns = Syntax.qualifiedIdentifierNamespace qi
          name = Syntax.qualifiedIdentifierName qi
      in (Serialization.cst (Strings.cat [
        ns,
        "::",
        name]))

-- | Convert a qualified type to an expression
encodeQualifiedType :: Syntax.QualifiedType -> Ast.Expr
encodeQualifiedType qt =

      let baseType = Syntax.qualifiedTypeBaseType qt
          qualifier = Syntax.qualifiedTypeQualifier qt
      in case qualifier of
        Syntax.TypeQualifierConst -> Serialization.spaceSep [
          Serialization.cst "const",
          (encodeTypeExpression baseType)]
        Syntax.TypeQualifierLvalueRef -> Serialization.noSep [
          encodeTypeExpression baseType,
          (Serialization.cst "&")]
        Syntax.TypeQualifierRvalueRef -> Serialization.noSep [
          encodeTypeExpression baseType,
          (Serialization.cst "&&")]
        Syntax.TypeQualifierPointer -> Serialization.noSep [
          encodeTypeExpression baseType,
          (Serialization.cst "*")]

-- | Convert a range-for statement to an expression
encodeRangeForStatement :: Syntax.RangeForStatement -> Ast.Expr
encodeRangeForStatement rfs =

      let typ = Syntax.rangeForStatementType rfs
          var = Syntax.rangeForStatementVariable rfs
          range = Syntax.rangeForStatementRange rfs
          body = Syntax.rangeForStatementBody rfs
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "for",
          (Serialization.parens (Serialization.spaceSep [
            encodeTypeExpression typ,
            (Serialization.cst var),
            (Serialization.cst ":"),
            (encodeExpression range)]))],
        (encodeStatement body)])

-- | Convert a relational expression to an expression
encodeRelationalExpression :: Syntax.RelationalExpression -> Ast.Expr
encodeRelationalExpression e =
    case e of
      Syntax.RelationalExpressionShift v0 -> encodeShiftExpression v0
      Syntax.RelationalExpressionLess v0 -> encodeLessOperation v0
      Syntax.RelationalExpressionGreater v0 -> encodeGreaterOperation v0
      Syntax.RelationalExpressionLessEqual v0 -> encodeLessEqualOperation v0
      Syntax.RelationalExpressionGreaterEqual v0 -> encodeGreaterEqualOperation v0

-- | Convert a right shift operation to an expression
encodeRightShiftOperation :: Syntax.RightShiftOperation -> Ast.Expr
encodeRightShiftOperation op =

      let left = Syntax.rightShiftOperationLeft op
          right = Syntax.rightShiftOperationRight op
      in (Serialization.spaceSep [
        encodeShiftExpression left,
        (Serialization.cst ">>"),
        (encodeAdditiveExpression right)])

-- | Convert a selection statement to an expression
encodeSelectionStatement :: Syntax.SelectionStatement -> Ast.Expr
encodeSelectionStatement ss =

      let cond = Syntax.selectionStatementCondition ss
          thenBranch = Syntax.selectionStatementThenBranch ss
          elseBranch = Syntax.selectionStatementElseBranch ss
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "if",
          (Serialization.parens (encodeExpression cond))],
        (encodeStatement thenBranch),
        (Maybes.maybe (Serialization.cst "") (\stmt -> Serialization.newlineSep [
          Serialization.cst "else",
          (encodeStatement stmt)]) elseBranch)])

-- | Convert a set to an expression
encodeSet :: Syntax.Set -> Ast.Expr
encodeSet s =

      let elemType = Syntax.setElementType s
          elems = Syntax.setElements s
      in (Serialization.spaceSep [
        Serialization.cst "std::set<",
        (encodeTypeExpression elemType),
        (Serialization.cst ">"),
        (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map encodeExpression elems))])

-- | Convert a shift expression to an expression
encodeShiftExpression :: Syntax.ShiftExpression -> Ast.Expr
encodeShiftExpression e =
    case e of
      Syntax.ShiftExpressionAdditive v0 -> encodeAdditiveExpression v0
      Syntax.ShiftExpressionLeftShift v0 -> encodeLeftShiftOperation v0
      Syntax.ShiftExpressionRightShift v0 -> encodeRightShiftOperation v0

-- | Convert a sizeof expression to an expression
encodeSizeofExpression :: Syntax.SizeofExpression -> Ast.Expr
encodeSizeofExpression se =
    Serialization.spaceSep [
      Serialization.cst "sizeof",
      (Serialization.parens (encodeTypeExpression (Syntax.unSizeofExpression se)))]

-- | Convert a statement to an expression
encodeStatement :: Syntax.Statement -> Ast.Expr
encodeStatement s =
    case s of
      Syntax.StatementLabeled v0 -> encodeLabeledStatement v0
      Syntax.StatementCompound v0 -> encodeCompoundStatement v0
      Syntax.StatementSelection v0 -> encodeSelectionStatement v0
      Syntax.StatementSwitch v0 -> encodeSwitchStatement v0
      Syntax.StatementIteration v0 -> encodeIterationStatement v0
      Syntax.StatementJump v0 -> encodeJumpStatement v0
      Syntax.StatementDeclaration v0 -> Serialization.withSemi (encodeVariableDeclaration False v0)
      Syntax.StatementExpression v0 -> Serialization.withSemi (encodeExpression v0)

-- | Convert a subscript operation to an expression
encodeSubscriptOperation :: Syntax.SubscriptOperation -> Ast.Expr
encodeSubscriptOperation so =

      let array = Syntax.subscriptOperationArray so
          index = Syntax.subscriptOperationIndex so
      in (Serialization.noSep [
        encodePostfixExpression array,
        (Serialization.cst "["),
        (encodeExpression index),
        (Serialization.cst "]")])

-- | Convert a subtract operation to an expression
encodeSubtractOperation :: Syntax.SubtractOperation -> Ast.Expr
encodeSubtractOperation op =

      let left = Syntax.subtractOperationLeft op
          right = Syntax.subtractOperationRight op
      in (Serialization.spaceSep [
        encodeAdditiveExpression left,
        (Serialization.cst "-"),
        (encodeMultiplicativeExpression right)])

-- | Convert a switch statement to an expression
encodeSwitchStatement :: Syntax.SwitchStatement -> Ast.Expr
encodeSwitchStatement ss =

      let value = Syntax.switchStatementValue ss
          cases = Syntax.switchStatementCases ss
      in (Serialization.spaceSep [
        Serialization.cst "switch",
        (Serialization.parens (encodeExpression value)),
        (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map encodeCaseStatement cases)))])

-- | Convert a template argument to an expression
encodeTemplateArgument :: Syntax.TemplateArgument -> Ast.Expr
encodeTemplateArgument a =
    case a of
      Syntax.TemplateArgumentType v0 -> encodeTypeExpression v0
      Syntax.TemplateArgumentValue v0 -> encodeExpression v0

-- | Convert a template declaration to an expression
encodeTemplateDeclaration :: Syntax.TemplateDeclaration -> Ast.Expr
encodeTemplateDeclaration td =

      let inline = Syntax.templateDeclarationInline td
          params = Syntax.templateDeclarationParameters td
          declaration = Syntax.templateDeclarationDeclaration td
          sep = Logic.ifElse inline Serialization.spaceSep Serialization.newlineSep
      in (sep [
        Serialization.noSep [
          Serialization.cst "template",
          (Serialization.angleBracesList Serialization.inlineStyle (Lists.map (\p -> Serialization.cst p) params))],
        (encodeDeclaration declaration)])

-- | Convert a template function call operation to an expression
encodeTemplateFunctionCallOperation :: Syntax.TemplateFunctionCallOperation -> Ast.Expr
encodeTemplateFunctionCallOperation tfco =

      let func = Syntax.templateFunctionCallOperationFunction tfco
          templateArgs = Syntax.templateFunctionCallOperationTemplateArguments tfco
          args = Syntax.templateFunctionCallOperationArguments tfco
      in (Serialization.noSep [
        encodePostfixExpression func,
        (Serialization.angleBracesList Serialization.inlineStyle (Lists.map encodeTemplateArgument templateArgs)),
        (Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeExpression args)))])

-- | Convert a template type to an expression
encodeTemplateType :: Syntax.TemplateType -> Ast.Expr
encodeTemplateType tt =

      let name = Syntax.templateTypeName tt
          args = Syntax.templateTypeArguments tt
      in (Serialization.noSep [
        Serialization.cst name,
        (Serialization.angleBracesList Serialization.inlineStyle (Lists.map encodeTemplateArgument args))])

-- | Convert a ternary expression to an expression
encodeTernaryExpression :: Syntax.TernaryExpression -> Ast.Expr
encodeTernaryExpression te =

      let cond = Syntax.ternaryExpressionCondition te
          trueExpr = Syntax.ternaryExpressionTrueExpr te
          falseExpr = Syntax.ternaryExpressionFalseExpr te
      in (Serialization.spaceSep [
        encodeLogicalOrExpression cond,
        (Serialization.cst "?"),
        (encodeExpression trueExpr),
        (Serialization.cst ":"),
        (encodeConditionalExpression falseExpr)])

-- | Convert a type expression to an expression
encodeTypeExpression :: Syntax.TypeExpression -> Ast.Expr
encodeTypeExpression t =
    case t of
      Syntax.TypeExpressionBasic v0 -> encodeBasicType v0
      Syntax.TypeExpressionQualified v0 -> encodeQualifiedType v0
      Syntax.TypeExpressionTemplate v0 -> encodeTemplateType v0
      Syntax.TypeExpressionFunction v0 -> encodeFunctionType v0
      Syntax.TypeExpressionAuto -> Serialization.cst "auto"

-- | Convert a typedef declaration to an expression
encodeTypedefDeclaration :: Syntax.TypedefDeclaration -> Ast.Expr
encodeTypedefDeclaration td =

      let name = Syntax.typedefDeclarationName td
          typ = Syntax.typedefDeclarationType td
          isUsing = Syntax.typedefDeclarationIsUsing td
      in (Logic.ifElse isUsing (Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "using " name),
        (Serialization.cst "="),
        (encodeTypeExpression typ)])) (Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "typedef",
        (encodeTypeExpression typ),
        (Serialization.cst name)])))

-- | Convert a unary expression to an expression
encodeUnaryExpression :: Syntax.UnaryExpression -> Ast.Expr
encodeUnaryExpression e =
    case e of
      Syntax.UnaryExpressionPostfix v0 -> encodePostfixExpression v0
      Syntax.UnaryExpressionUnaryOp v0 -> encodeUnaryOperation v0
      Syntax.UnaryExpressionSizeof v0 -> encodeSizeofExpression v0

-- | Convert a unary operation to an expression
encodeUnaryOperation :: Syntax.UnaryOperation -> Ast.Expr
encodeUnaryOperation uo =

      let op = Syntax.unaryOperationOperator uo
          operand = Syntax.unaryOperationOperand uo
      in (Serialization.spaceSep [
        encodeUnaryOperator op,
        (encodeUnaryExpression operand)])

-- | Convert a unary operator to an expression
encodeUnaryOperator :: Syntax.UnaryOperator -> Ast.Expr
encodeUnaryOperator op =
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
encodeUndefDirective :: Syntax.UndefDirective -> Ast.Expr
encodeUndefDirective ud =
    Serialization.spaceSep [
      Serialization.cst "#undef",
      (Serialization.cst (Syntax.undefDirectiveName ud))]

-- | Convert a variable declaration to an expression
encodeVariableDeclaration :: Bool -> Syntax.VariableDeclaration -> Ast.Expr
encodeVariableDeclaration commas vd =

      let typ = Syntax.variableDeclarationType vd
          name = Syntax.variableDeclarationName vd
          init = Syntax.variableDeclarationInitializer vd
          isAuto = Syntax.variableDeclarationIsAuto vd
          terminator = Logic.ifElse commas Serialization.withComma Serialization.withSemi
      in (terminator (Serialization.spaceSep (Lists.concat [
        Logic.ifElse isAuto [
          Serialization.cst "auto"] (Maybes.maybe [] (\t -> [
          encodeTypeExpression t]) typ),
        [
          Serialization.cst name],
        (Maybes.maybe [] (\expr -> [
          Serialization.cst "=",
          (encodeExpression expr)]) init)])))

-- | Convert a vector to an expression
encodeVector :: Syntax.Vector -> Ast.Expr
encodeVector v =

      let elemType = Syntax.vectorElementType v
          elems = Syntax.vectorElements v
      in (Serialization.spaceSep [
        Serialization.cst "std::vector<",
        (encodeTypeExpression elemType),
        (Serialization.cst ">"),
        (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map encodeExpression elems))])

-- | Convert a visitor to an expression
encodeVisitor :: Syntax.Visitor -> Ast.Expr
encodeVisitor v =
    case v of
      Syntax.VisitorLambda v0 -> encodeLambdaExpression v0
      Syntax.VisitorOverloaded v0 -> encodeOverloadedLambdas v0

-- | Convert a warning directive to an expression
encodeWarningDirective :: Syntax.WarningDirective -> Ast.Expr
encodeWarningDirective wd =
    Serialization.spaceSep [
      Serialization.cst "#warning",
      (Serialization.cst (Syntax.warningDirectiveMessage wd))]

-- | Convert a while statement to an expression
encodeWhileStatement :: Syntax.WhileStatement -> Ast.Expr
encodeWhileStatement ws =

      let cond = Syntax.whileStatementCondition ws
          body = Syntax.whileStatementBody ws
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "while",
          (Serialization.parens (encodeExpression cond))],
        (encodeStatement body)])

-- | Convert a string to a C++ comment
toCppComments :: String -> Bool -> String
toCppComments s isMultiline =
    Logic.ifElse isMultiline (Strings.cat [
      "/* ",
      s,
      " */"]) (Strings.cat2 "// " s)
