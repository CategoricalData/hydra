-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Cpp.Serde where

import qualified Hydra.Ext.Cpp.Syntax as Cpp
import Hydra.Staging.Serialization
import qualified Hydra.Ast as A

import qualified Data.List as L
import qualified Data.Maybe as Y


-- Program structure
encodeProgram :: Cpp.Program -> A.Expr
encodeProgram (Cpp.Program preps includes decls) = doubleNewlineSep $ Y.catMaybes [
    separate newlineSep (encodePreprocessorDirective <$> preps),
    separate newlineSep (encodeIncludeDirective <$> includes),
    separate doubleNewlineSep (encodeDeclaration <$> decls)]
  where
    separate sep defs = if L.null defs
      then Nothing
      else Just $ sep defs

encodePreprocessorDirective :: Cpp.PreprocessorDirective -> A.Expr
encodePreprocessorDirective d = case d of
  Cpp.PreprocessorDirectiveInclude i -> encodeIncludeDirective i
  Cpp.PreprocessorDirectivePragma p -> encodePragmaDirective p
  Cpp.PreprocessorDirectiveDefine d -> encodeDefineDirective d
  Cpp.PreprocessorDirectiveUndef u -> encodeUndefDirective u
  Cpp.PreprocessorDirectiveIfdef i -> encodeIfdefDirective i
  Cpp.PreprocessorDirectiveIfndef i -> encodeIfndefDirective i
  Cpp.PreprocessorDirectiveIf i -> encodeIfDirective i
  Cpp.PreprocessorDirectiveElif e -> encodeElifDirective e
  Cpp.PreprocessorDirectiveElse e -> encodeElseDirective e
  Cpp.PreprocessorDirectiveEndif e -> encodeEndifDirective e
  Cpp.PreprocessorDirectiveLine l -> encodeLineDirective l
  Cpp.PreprocessorDirectiveError e -> encodeErrorDirective e
  Cpp.PreprocessorDirectiveWarning w -> encodeWarningDirective w

encodePragmaDirective :: Cpp.PragmaDirective -> A.Expr
encodePragmaDirective (Cpp.PragmaDirective content) =
  cst $ "#pragma " ++ content

encodeDefineDirective :: Cpp.DefineDirective -> A.Expr
encodeDefineDirective (Cpp.DefineDirective name params replacement) =
  spaceSep $ [cst "#define", cst name] ++
    (case params of
      Just ps -> [parens $ commaSep inlineStyle $ cst <$> ps]
      Nothing -> []) ++
    (case replacement of
      Just r -> [cst r]
      Nothing -> [])

encodeUndefDirective :: Cpp.UndefDirective -> A.Expr
encodeUndefDirective (Cpp.UndefDirective name) =
  spaceSep [cst "#undef", cst name]

encodeIfdefDirective :: Cpp.IfdefDirective -> A.Expr
encodeIfdefDirective (Cpp.IfdefDirective identifier) =
  spaceSep [cst "#ifdef", cst identifier]

encodeIfndefDirective :: Cpp.IfndefDirective -> A.Expr
encodeIfndefDirective (Cpp.IfndefDirective identifier) =
  spaceSep [cst "#ifndef", cst identifier]

encodeIfDirective :: Cpp.IfDirective -> A.Expr
encodeIfDirective (Cpp.IfDirective condition) =
  spaceSep [cst "#if", cst condition]

encodeElifDirective :: Cpp.ElifDirective -> A.Expr
encodeElifDirective (Cpp.ElifDirective condition) =
  spaceSep [cst "#elif", cst condition]

encodeElseDirective :: Cpp.ElseDirective -> A.Expr
encodeElseDirective _ = cst "#else"

encodeEndifDirective :: Cpp.EndifDirective -> A.Expr
encodeEndifDirective _ = cst "#endif"

encodeLineDirective :: Cpp.LineDirective -> A.Expr
encodeLineDirective (Cpp.LineDirective lineNumber filename) =
  spaceSep $ [cst "#line", cst $ show lineNumber] ++
    (case filename of
      Just f -> [cst $ "\"" ++ f ++ "\""]
      Nothing -> [])

encodeErrorDirective :: Cpp.ErrorDirective -> A.Expr
encodeErrorDirective (Cpp.ErrorDirective message) =
  spaceSep [cst "#error", cst message]

encodeWarningDirective :: Cpp.WarningDirective -> A.Expr
encodeWarningDirective (Cpp.WarningDirective message) =
  spaceSep [cst "#warning", cst message]

encodeIncludeDirective :: Cpp.IncludeDirective -> A.Expr
encodeIncludeDirective (Cpp.IncludeDirective name isSystem) =
  if isSystem
    then cst $ "#include <" ++ name ++ ">"
    else cst $ "#include \"" ++ name ++ "\""

encodeDeclaration :: Cpp.Declaration -> A.Expr
encodeDeclaration d = case d of
  Cpp.DeclarationPreprocessor p -> encodePreprocessorDirective p
  Cpp.DeclarationClass c -> encodeClassDeclaration c
  Cpp.DeclarationFunction f -> encodeFunctionDeclaration f
  Cpp.DeclarationVariable v -> encodeVariableDeclaration False v
  Cpp.DeclarationTypedef t -> encodeTypedefDeclaration t
  Cpp.DeclarationNamespace n -> encodeNamespaceDeclaration n
  Cpp.DeclarationTemplate t -> encodeTemplateDeclaration t

encodeNamespaceDeclaration :: Cpp.NamespaceDeclaration -> A.Expr
encodeNamespaceDeclaration (Cpp.NamespaceDeclaration name decls) =
  spaceSep [
    cst $ "namespace " ++ name,
    curlyBlock fullBlockStyle (doubleNewlineSep $ encodeDeclaration <$> decls)]

encodeTypedefDeclaration :: Cpp.TypedefDeclaration -> A.Expr
encodeTypedefDeclaration (Cpp.TypedefDeclaration name typ isUsing) =
  if isUsing
    then withSemi $ spaceSep [cst $ "using " ++ name, cst "=", encodeTypeExpression typ]
    else withSemi $ spaceSep [cst "typedef", encodeTypeExpression typ, cst name]

-- Class-related encoders
encodeClassBody :: Bool -> Cpp.ClassBody -> A.Expr
encodeClassBody commas (Cpp.ClassBody members) = curlyBlock fullBlockStyle (doubleNewlineSep $ map (encodeMemberSpecification commas) members)

encodeClassDeclaration :: Cpp.ClassDeclaration -> A.Expr
encodeClassDeclaration (Cpp.ClassDeclaration spec mbody) = withSemi $
  spaceSep $ Y.catMaybes [
    Just $ encodeClassSpecifier spec,
    encodeClassBody isEnum <$> mbody]
  where
    isEnum = Cpp.classSpecifierKey spec == Cpp.ClassKeyEnum || Cpp.classSpecifierKey spec == Cpp.ClassKeyEnumClass

encodeClassSpecifier :: Cpp.ClassSpecifier -> A.Expr
encodeClassSpecifier (Cpp.ClassSpecifier key name inheritance) =
  spaceSep $ [encodeClassKey key, cst name] ++
    (if null inheritance
      then []
      else [cst ":", commaSep inlineStyle $ encodeBaseSpecifier <$> inheritance])

encodeClassKey :: Cpp.ClassKey -> A.Expr
encodeClassKey k = case k of
  Cpp.ClassKeyClass -> cst "class"
  Cpp.ClassKeyEnum -> cst "enum"
  Cpp.ClassKeyEnumClass -> cst "enum class"
  Cpp.ClassKeyStruct -> cst "struct"

encodeBaseSpecifier :: Cpp.BaseSpecifier -> A.Expr
encodeBaseSpecifier (Cpp.BaseSpecifier access name) =
  spaceSep [encodeAccessSpecifier access, cst name]

encodeAccessSpecifier :: Cpp.AccessSpecifier -> A.Expr
encodeAccessSpecifier a = case a of
  Cpp.AccessSpecifierPublic -> cst "public"
  Cpp.AccessSpecifierProtected -> cst "protected"
  Cpp.AccessSpecifierPrivate -> cst "private"
  Cpp.AccessSpecifierNone -> cst ""

encodeMemberSpecification :: Bool -> Cpp.MemberSpecification -> A.Expr
encodeMemberSpecification commas m = case m of
  Cpp.MemberSpecificationAccessLabel a -> noSep [encodeAccessSpecifier a, cst ":"]
  Cpp.MemberSpecificationMember d -> encodeMemberDeclaration commas d

encodeMemberDeclaration :: Bool -> Cpp.MemberDeclaration -> A.Expr
encodeMemberDeclaration commas m = case m of
  Cpp.MemberDeclarationFunction f -> encodeFunctionDeclaration f
  Cpp.MemberDeclarationVariable v -> encodeVariableDeclaration commas v
  Cpp.MemberDeclarationConstructor c -> encodeConstructorDeclaration c
  Cpp.MemberDeclarationDestructor d -> encodeDestructorDeclaration d
  Cpp.MemberDeclarationNestedClass c -> encodeClassDeclaration c
  Cpp.MemberDeclarationTemplate t -> encodeTemplateDeclaration t

encodeConstructorDeclaration :: Cpp.ConstructorDeclaration -> A.Expr
encodeConstructorDeclaration (Cpp.ConstructorDeclaration name params inits body) =
  spaceSep $ Y.catMaybes [
    Just $ noSep [
      cst name,
      parens $ commaSep inlineStyle $ encodeParameter <$> params],
    if null inits
      then Nothing
      else Just $ spaceSep [
        cst ":",
        commaSep inlineStyle $ encodeMemInitializer <$> inits],
    Just $ encodeFunctionBody body]

encodeMemInitializer :: Cpp.MemInitializer -> A.Expr
encodeMemInitializer (Cpp.MemInitializer name args) =
  noSep [
    cst name,
    parens $ commaSep inlineStyle $ encodeExpression <$> args]

encodeDestructorDeclaration :: Cpp.DestructorDeclaration -> A.Expr
encodeDestructorDeclaration (Cpp.DestructorDeclaration prefixSpecs name suffixSpecs body) =
  spaceSep $
    fmap encodeFunctionSpecifierPrefix prefixSpecs ++
    [noSep [
      cst $ "~" ++ name,
      parens $ cst ""]] ++
    fmap encodeFunctionSpecifierSuffix suffixSpecs ++
    [encodeFunctionBody body]

encodeFunctionDeclaration :: Cpp.FunctionDeclaration -> A.Expr
encodeFunctionDeclaration (Cpp.FunctionDeclaration prefixSpecs retType name params suffixSpecs body) =
  spaceSep $
    fmap encodeFunctionSpecifierPrefix prefixSpecs ++ [
      encodeTypeExpression retType,
      noSep [
        cst name,
        parens $ commaSep inlineStyle $ fmap encodeParameter params]] ++
    fmap encodeFunctionSpecifierSuffix suffixSpecs ++ [
      encodeFunctionBody body]

encodeFunctionSpecifierPrefix :: Cpp.FunctionSpecifierPrefix -> A.Expr
encodeFunctionSpecifierPrefix s = case s of
  Cpp.FunctionSpecifierPrefixVirtual -> cst "virtual"
  Cpp.FunctionSpecifierPrefixStatic  -> cst "static"
  Cpp.FunctionSpecifierPrefixExplicit -> cst "explicit"

encodeFunctionSpecifierSuffix :: Cpp.FunctionSpecifierSuffix -> A.Expr
encodeFunctionSpecifierSuffix s = case s of
  Cpp.FunctionSpecifierSuffixConst    -> cst "const"
  Cpp.FunctionSpecifierSuffixNoexcept -> cst "noexcept"
  Cpp.FunctionSpecifierSuffixOverride -> cst "override"
  Cpp.FunctionSpecifierSuffixFinal    -> cst "final"

encodeParameter :: Cpp.Parameter -> A.Expr
encodeParameter (Cpp.Parameter typ name unnamed defaultVal) =
  spaceSep $ [
    encodeTypeExpression typ,
    nameExpr] ++
    (case defaultVal of
      Just expr -> [cst "=", encodeExpression expr]
      Nothing -> [])
  where
    nameExpr = cst $ if unnamed
      then "/*" ++ name ++ "*/"
      else name

encodeFunctionBody :: Cpp.FunctionBody -> A.Expr
encodeFunctionBody b = case b of
  Cpp.FunctionBodyCompound c -> encodeCompoundStatement c
  Cpp.FunctionBodyDeclaration -> cst ";"
  Cpp.FunctionBodyPure -> withSemi $ cst "= 0"
  Cpp.FunctionBodyDefault -> withSemi $ cst "= default"

-- Variable and type declarations
encodeVariableDeclaration :: Bool -> Cpp.VariableDeclaration -> A.Expr
encodeVariableDeclaration commas (Cpp.VariableDeclaration typ name init isAuto) =
  (if commas then withComma else withSemi) $ spaceSep $
    (if isAuto
      then [cst "auto"]
      else (Y.maybe [] (\t -> [encodeTypeExpression t]) typ)) ++
    [cst name] ++
    (case init of
      Just expr -> [cst "=", encodeExpression expr]
      Nothing -> [])

-- Types
encodeTypeExpression :: Cpp.TypeExpression -> A.Expr
encodeTypeExpression t = case t of
  Cpp.TypeExpressionBasic b -> encodeBasicType b
  Cpp.TypeExpressionQualified q -> encodeQualifiedType q
  Cpp.TypeExpressionTemplate t -> encodeTemplateType t
  Cpp.TypeExpressionFunction f -> encodeFunctionType f
  Cpp.TypeExpressionAuto -> cst "auto"

encodeBasicType :: Cpp.BasicType -> A.Expr
encodeBasicType t = case t of
  Cpp.BasicTypeVoid -> cst "void"
  Cpp.BasicTypeBool -> cst "bool"
  Cpp.BasicTypeChar -> cst "char"
  Cpp.BasicTypeInt -> cst "int"
  Cpp.BasicTypeFloat -> cst "float"
  Cpp.BasicTypeDouble -> cst "double"
  Cpp.BasicTypeString -> cst "std::string"
  Cpp.BasicTypeAuto -> cst "auto"
  Cpp.BasicTypeNamed name -> cst name

encodeQualifiedType :: Cpp.QualifiedType -> A.Expr
encodeQualifiedType (Cpp.QualifiedType baseType qualifier) =
  case qualifier of
    Cpp.TypeQualifierConst -> spaceSep [cst "const", encodeTypeExpression baseType]
    Cpp.TypeQualifierLvalueRef -> noSep [encodeTypeExpression baseType, cst "&"]
    Cpp.TypeQualifierRvalueRef -> noSep [encodeTypeExpression baseType, cst "&&"]
    Cpp.TypeQualifierPointer -> noSep [encodeTypeExpression baseType, cst "*"]

encodeTemplateDeclaration :: Cpp.TemplateDeclaration -> A.Expr
encodeTemplateDeclaration (Cpp.TemplateDeclaration inline params declaration) =
  (if inline then spaceSep else newlineSep) [
    noSep [
      cst "template",
      angleBracesList inlineStyle $ cst <$> params],
    encodeDeclaration declaration]

encodeTemplateType :: Cpp.TemplateType -> A.Expr
encodeTemplateType (Cpp.TemplateType name args) =
  noSep [
    cst name,
    angleBracesList inlineStyle $ encodeTemplateArgument <$> args]

encodeTemplateArgument :: Cpp.TemplateArgument -> A.Expr
encodeTemplateArgument a = case a of
  Cpp.TemplateArgumentType t -> encodeTypeExpression t
  Cpp.TemplateArgumentValue e -> encodeExpression e

encodeFunctionType :: Cpp.FunctionType -> A.Expr
encodeFunctionType (Cpp.FunctionType retType params) =
  spaceSep [
    encodeTypeExpression retType,
    parens $ commaSep inlineStyle $ encodeParameter <$> params]

-- Statements
encodeStatement :: Cpp.Statement -> A.Expr
encodeStatement s = case s of
  Cpp.StatementLabeled l -> encodeLabeledStatement l
  Cpp.StatementCompound c -> encodeCompoundStatement c
  Cpp.StatementSelection s -> encodeSelectionStatement s
  Cpp.StatementSwitch s -> encodeSwitchStatement s
  Cpp.StatementIteration i -> encodeIterationStatement i
  Cpp.StatementJump j -> encodeJumpStatement j
  Cpp.StatementDeclaration v -> withSemi $ encodeVariableDeclaration False v
  Cpp.StatementExpression e -> withSemi $ encodeExpression e

encodeLabeledStatement :: Cpp.LabeledStatement -> A.Expr
encodeLabeledStatement (Cpp.LabeledStatement label stmt) =
  newlineSep [
    cst $ label ++ ":",
    encodeStatement stmt]

encodeCompoundStatement :: Cpp.CompoundStatement -> A.Expr
encodeCompoundStatement (Cpp.CompoundStatement stmts) =
  curlyBracesList (Just "") fullBlockStyle (encodeStatement <$> stmts)

encodeSelectionStatement :: Cpp.SelectionStatement -> A.Expr
encodeSelectionStatement (Cpp.SelectionStatement cond thenBranch elseBranch) =
  newlineSep [
    spaceSep [
      cst "if",
      parens $ encodeExpression cond],
    encodeStatement thenBranch,
    case elseBranch of
      Just stmt -> newlineSep [cst "else", encodeStatement stmt]
      Nothing -> cst ""]

encodeSwitchStatement :: Cpp.SwitchStatement -> A.Expr
encodeSwitchStatement (Cpp.SwitchStatement value cases) =
  spaceSep [
    cst "switch",
    parens $ encodeExpression value,
    curlyBlock fullBlockStyle $ newlineSep $ map encodeCaseStatement cases]

encodeCaseStatement :: Cpp.CaseStatement -> A.Expr
encodeCaseStatement stmt = case stmt of
  Cpp.CaseStatementCase caseValue ->
    encodeCaseValue caseValue
  Cpp.CaseStatementDefault statement ->
    spaceSep [
      cst "default:",
      encodeStatement statement]

encodeCaseValue :: Cpp.CaseValue -> A.Expr
encodeCaseValue (Cpp.CaseValue value statement) =
  spaceSep [
    cst "case",
    noSep [encodeExpression value, cst ":"],
    encodeStatement statement]

encodeIterationStatement :: Cpp.IterationStatement -> A.Expr
encodeIterationStatement i = case i of
  Cpp.IterationStatementWhile w -> encodeWhileStatement w
  Cpp.IterationStatementDo d -> encodeDoStatement d
  Cpp.IterationStatementFor f -> encodeForStatement f
  Cpp.IterationStatementRangeFor r -> encodeRangeForStatement r

encodeWhileStatement :: Cpp.WhileStatement -> A.Expr
encodeWhileStatement (Cpp.WhileStatement cond body) =
  newlineSep [
    spaceSep [
      cst "while",
      parens $ encodeExpression cond],
    encodeStatement body]

encodeDoStatement :: Cpp.DoStatement -> A.Expr
encodeDoStatement (Cpp.DoStatement body cond) =
  newlineSep [
    cst "do",
    encodeStatement body,
    withSemi $ spaceSep [
      cst "while",
      parens $ encodeExpression cond]]

encodeForStatement :: Cpp.ForStatement -> A.Expr
encodeForStatement (Cpp.ForStatement init cond inc body) =
  newlineSep [
    spaceSep [
      cst "for",
      parens $ noSep [
        encodeForInit init,
        cst ";",
        encodeExpression cond,
        cst ";",
        encodeExpression inc]],
    encodeStatement body]

encodeForInit :: Cpp.ForInit -> A.Expr
encodeForInit i = case i of
  Cpp.ForInitExpression e -> encodeExpression e
  Cpp.ForInitDeclaration d -> encodeVariableDeclaration False d
  Cpp.ForInitEmpty -> cst ""

encodeRangeForStatement :: Cpp.RangeForStatement -> A.Expr
encodeRangeForStatement (Cpp.RangeForStatement typ var range body) =
  newlineSep [
    spaceSep [
      cst "for",
      parens $ spaceSep [
        encodeTypeExpression typ,
        cst var,
        cst ":",
        encodeExpression range]],
    encodeStatement body]

encodeJumpStatement :: Cpp.JumpStatement -> A.Expr
encodeJumpStatement j = case j of
  Cpp.JumpStatementBreak -> withSemi $ cst "break"
  Cpp.JumpStatementContinue -> withSemi $ cst "continue"
  Cpp.JumpStatementReturnValue e -> withSemi $ spaceSep [cst "return", encodeExpression e]
  Cpp.JumpStatementReturnVoid -> withSemi $ cst "return"
  Cpp.JumpStatementThrow e -> withSemi $ spaceSep [cst "throw", encodeExpression e]

-- Expressions
encodeExpression :: Cpp.Expression -> A.Expr
encodeExpression e = case e of
  Cpp.ExpressionAssignment a -> encodeAssignmentExpression a
  Cpp.ExpressionComma c -> encodeCommaExpression c

encodeCommaExpression :: Cpp.CommaExpression -> A.Expr
encodeCommaExpression (Cpp.CommaExpression left right) =
  spaceSep [encodeExpression left, cst ",", encodeAssignmentExpression right]

encodeAssignmentExpression :: Cpp.AssignmentExpression -> A.Expr
encodeAssignmentExpression a = case a of
  Cpp.AssignmentExpressionConditional c -> encodeConditionalExpression c
  Cpp.AssignmentExpressionAssignment e -> encodeExplicitAssignment e

encodeExplicitAssignment :: Cpp.ExplicitAssignment -> A.Expr
encodeExplicitAssignment (Cpp.ExplicitAssignment left op right) =
  spaceSep [
    encodeLogicalOrExpression left,
    encodeAssignmentOperator op,
    encodeAssignmentExpression right]

encodeAssignmentOperator :: Cpp.AssignmentOperator -> A.Expr
encodeAssignmentOperator op = case op of
  Cpp.AssignmentOperatorAssign -> cst "="
  Cpp.AssignmentOperatorPlusAssign -> cst "+="
  Cpp.AssignmentOperatorMinusAssign -> cst "-="
  Cpp.AssignmentOperatorMultiplyAssign -> cst "*="
  Cpp.AssignmentOperatorDivideAssign -> cst "/="
  Cpp.AssignmentOperatorModuloAssign -> cst "%="
  Cpp.AssignmentOperatorLeftShiftAssign -> cst "<<="
  Cpp.AssignmentOperatorRightShiftAssign -> cst ">>="
  Cpp.AssignmentOperatorBitwiseAndAssign -> cst "&="
  Cpp.AssignmentOperatorBitwiseXorAssign -> cst "^="
  Cpp.AssignmentOperatorBitwiseOrAssign -> cst "|="

encodeConditionalExpression :: Cpp.ConditionalExpression -> A.Expr
encodeConditionalExpression c = case c of
  Cpp.ConditionalExpressionLogicalOr l -> encodeLogicalOrExpression l
  Cpp.ConditionalExpressionTernary t -> encodeTernaryExpression t

encodeTernaryExpression :: Cpp.TernaryExpression -> A.Expr
encodeTernaryExpression (Cpp.TernaryExpression cond trueExpr falseExpr) =
  spaceSep [
    encodeLogicalOrExpression cond,
    cst "?",
    encodeExpression trueExpr,
    cst ":",
    encodeConditionalExpression falseExpr]

encodeLogicalOrExpression :: Cpp.LogicalOrExpression -> A.Expr
encodeLogicalOrExpression e = case e of
  Cpp.LogicalOrExpressionLogicalAnd l -> encodeLogicalAndExpression l
  Cpp.LogicalOrExpressionLogicalOr o -> encodeLogicalOrOperation o

encodeLogicalOrOperation :: Cpp.LogicalOrOperation -> A.Expr
encodeLogicalOrOperation (Cpp.LogicalOrOperation left right) =
  spaceSep [
    encodeLogicalOrExpression left,
    cst "||",
    encodeLogicalAndExpression right]

encodeLogicalAndExpression :: Cpp.LogicalAndExpression -> A.Expr
encodeLogicalAndExpression e = case e of
  Cpp.LogicalAndExpressionInclusiveOr i -> encodeInclusiveOrExpression i
  Cpp.LogicalAndExpressionLogicalAnd a -> encodeLogicalAndOperation a

encodeLogicalAndOperation :: Cpp.LogicalAndOperation -> A.Expr
encodeLogicalAndOperation (Cpp.LogicalAndOperation left right) =
  spaceSep [
    encodeLogicalAndExpression left,
    cst "&&",
    encodeInclusiveOrExpression right]

encodeInclusiveOrExpression :: Cpp.InclusiveOrExpression -> A.Expr
encodeInclusiveOrExpression e = case e of
  Cpp.InclusiveOrExpressionExclusiveOr x -> encodeExclusiveOrExpression x
  Cpp.InclusiveOrExpressionBitwiseOr o -> encodeBitwiseOrOperation o

encodeBitwiseOrOperation :: Cpp.BitwiseOrOperation -> A.Expr
encodeBitwiseOrOperation (Cpp.BitwiseOrOperation left right) =
  spaceSep [
    encodeInclusiveOrExpression left,
    cst "|",
    encodeExclusiveOrExpression right]

encodeExclusiveOrExpression :: Cpp.ExclusiveOrExpression -> A.Expr
encodeExclusiveOrExpression e = case e of
  Cpp.ExclusiveOrExpressionAnd a -> encodeAndExpression a
  Cpp.ExclusiveOrExpressionBitwiseXor x -> encodeBitwiseXorOperation x

encodeBitwiseXorOperation :: Cpp.BitwiseXorOperation -> A.Expr
encodeBitwiseXorOperation (Cpp.BitwiseXorOperation left right) =
  spaceSep [
    encodeExclusiveOrExpression left,
    cst "^",
    encodeAndExpression right]

encodeAndExpression :: Cpp.AndExpression -> A.Expr
encodeAndExpression e = case e of
  Cpp.AndExpressionEquality eq -> encodeEqualityExpression eq
  Cpp.AndExpressionBitwiseAnd a -> encodeBitwiseAndOperation a

encodeBitwiseAndOperation :: Cpp.BitwiseAndOperation -> A.Expr
encodeBitwiseAndOperation (Cpp.BitwiseAndOperation left right) =
  spaceSep [
    encodeAndExpression left,
    cst "&",
    encodeEqualityExpression right]

encodeEqualityExpression :: Cpp.EqualityExpression -> A.Expr
encodeEqualityExpression e = case e of
  Cpp.EqualityExpressionRelational r -> encodeRelationalExpression r
  Cpp.EqualityExpressionEqual eq -> encodeEqualOperation eq
  Cpp.EqualityExpressionNotEqual ne -> encodeNotEqualOperation ne

encodeEqualOperation :: Cpp.EqualOperation -> A.Expr
encodeEqualOperation (Cpp.EqualOperation left right) =
  spaceSep [
    encodeEqualityExpression left,
    cst "==",
    encodeRelationalExpression right]

encodeNotEqualOperation :: Cpp.NotEqualOperation -> A.Expr
encodeNotEqualOperation (Cpp.NotEqualOperation left right) =
  spaceSep [
    encodeEqualityExpression left,
    cst "!=",
    encodeRelationalExpression right]

encodeRelationalExpression :: Cpp.RelationalExpression -> A.Expr
encodeRelationalExpression e = case e of
  Cpp.RelationalExpressionShift s -> encodeShiftExpression s
  Cpp.RelationalExpressionLess l -> encodeLessOperation l
  Cpp.RelationalExpressionGreater g -> encodeGreaterOperation g
  Cpp.RelationalExpressionLessEqual le -> encodeLessEqualOperation le
  Cpp.RelationalExpressionGreaterEqual ge -> encodeGreaterEqualOperation ge

encodeLessOperation :: Cpp.LessOperation -> A.Expr
encodeLessOperation (Cpp.LessOperation left right) =
  spaceSep [
    encodeRelationalExpression left,
    cst "<",
    encodeShiftExpression right]

encodeGreaterOperation :: Cpp.GreaterOperation -> A.Expr
encodeGreaterOperation (Cpp.GreaterOperation left right) =
  spaceSep [
    encodeRelationalExpression left,
    cst ">",
    encodeShiftExpression right]

encodeLessEqualOperation :: Cpp.LessEqualOperation -> A.Expr
encodeLessEqualOperation (Cpp.LessEqualOperation left right) =
  spaceSep [
    encodeRelationalExpression left,
    cst "<=",
    encodeShiftExpression right]

encodeGreaterEqualOperation :: Cpp.GreaterEqualOperation -> A.Expr
encodeGreaterEqualOperation (Cpp.GreaterEqualOperation left right) =
  spaceSep [
    encodeRelationalExpression left,
    cst ">=",
    encodeShiftExpression right]

encodeShiftExpression :: Cpp.ShiftExpression -> A.Expr
encodeShiftExpression e = case e of
  Cpp.ShiftExpressionAdditive a -> encodeAdditiveExpression a
  Cpp.ShiftExpressionLeftShift ls -> encodeLeftShiftOperation ls
  Cpp.ShiftExpressionRightShift rs -> encodeRightShiftOperation rs

encodeLeftShiftOperation :: Cpp.LeftShiftOperation -> A.Expr
encodeLeftShiftOperation (Cpp.LeftShiftOperation left right) =
  spaceSep [
    encodeShiftExpression left,
    cst "<<",
    encodeAdditiveExpression right]

encodeRightShiftOperation :: Cpp.RightShiftOperation -> A.Expr
encodeRightShiftOperation (Cpp.RightShiftOperation left right) =
  spaceSep [
    encodeShiftExpression left,
    cst ">>",
    encodeAdditiveExpression right]

encodeAdditiveExpression :: Cpp.AdditiveExpression -> A.Expr
encodeAdditiveExpression e = case e of
  Cpp.AdditiveExpressionMultiplicative m -> encodeMultiplicativeExpression m
  Cpp.AdditiveExpressionAdd a -> encodeAddOperation a
  Cpp.AdditiveExpressionSubtract s -> encodeSubtractOperation s

encodeAddOperation :: Cpp.AddOperation -> A.Expr
encodeAddOperation (Cpp.AddOperation left right) =
  spaceSep [
    encodeAdditiveExpression left,
    cst "+",
    encodeMultiplicativeExpression right]

encodeSubtractOperation :: Cpp.SubtractOperation -> A.Expr
encodeSubtractOperation (Cpp.SubtractOperation left right) =
  spaceSep [
    encodeAdditiveExpression left,
    cst "-",
    encodeMultiplicativeExpression right]

encodeMultiplicativeExpression :: Cpp.MultiplicativeExpression -> A.Expr
encodeMultiplicativeExpression e = case e of
  Cpp.MultiplicativeExpressionUnary u -> encodeUnaryExpression u
  Cpp.MultiplicativeExpressionMultiply m -> encodeMultiplyOperation m
  Cpp.MultiplicativeExpressionDivide d -> encodeDivideOperation d
  Cpp.MultiplicativeExpressionModulo m -> encodeModuloOperation m

encodeMultiplyOperation :: Cpp.MultiplyOperation -> A.Expr
encodeMultiplyOperation (Cpp.MultiplyOperation left right) =
  spaceSep [
    encodeMultiplicativeExpression left,
    cst "*",
    encodeUnaryExpression right]

encodeDivideOperation :: Cpp.DivideOperation -> A.Expr
encodeDivideOperation (Cpp.DivideOperation left right) =
  spaceSep [
    encodeMultiplicativeExpression left,
    cst "/",
    encodeUnaryExpression right]

encodeModuloOperation :: Cpp.ModuloOperation -> A.Expr
encodeModuloOperation (Cpp.ModuloOperation left right) =
  spaceSep [
    encodeMultiplicativeExpression left,
    cst "%",
    encodeUnaryExpression right]

encodeSizeofExpression :: Cpp.SizeofExpression -> A.Expr
encodeSizeofExpression (Cpp.SizeofExpression typeExpr) =
  spaceSep [cst "sizeof", parens $ encodeTypeExpression typeExpr]

encodeUnaryExpression :: Cpp.UnaryExpression -> A.Expr
encodeUnaryExpression e = case e of
  Cpp.UnaryExpressionPostfix p -> encodePostfixExpression p
  Cpp.UnaryExpressionUnaryOp o -> encodeUnaryOperation o
  Cpp.UnaryExpressionSizeof s -> encodeSizeofExpression s

encodeUnaryOperation :: Cpp.UnaryOperation -> A.Expr
encodeUnaryOperation (Cpp.UnaryOperation op operand) =
  spaceSep [encodeUnaryOperator op, encodeUnaryExpression operand]

encodeUnaryOperator :: Cpp.UnaryOperator -> A.Expr
encodeUnaryOperator op = case op of
  Cpp.UnaryOperatorPlus -> cst "+"
  Cpp.UnaryOperatorMinus -> cst "-"
  Cpp.UnaryOperatorLogicalNot -> cst "!"
  Cpp.UnaryOperatorBitwiseNot -> cst "~"
  Cpp.UnaryOperatorDereference -> cst "*"
  Cpp.UnaryOperatorAddressOf -> cst "&"
  Cpp.UnaryOperatorPreIncrement -> cst "++"
  Cpp.UnaryOperatorPreDecrement -> cst "--"

encodePostfixExpression :: Cpp.PostfixExpression -> A.Expr
encodePostfixExpression e = case e of
  Cpp.PostfixExpressionPrimary p -> encodePrimaryExpression p
  Cpp.PostfixExpressionSubscript s -> encodeSubscriptOperation s
  Cpp.PostfixExpressionFunctionCall f -> encodeFunctionCallOperation f
  Cpp.PostfixExpressionTemplateFunctionCall t -> encodeTemplateFunctionCallOperation t
  Cpp.PostfixExpressionMemberAccess m -> encodeMemberAccessOperation m
  Cpp.PostfixExpressionPointerMemberAccess p -> encodePointerMemberAccessOperation p
  Cpp.PostfixExpressionPostIncrement p -> noSep [encodePostfixExpression p, cst "++"]
  Cpp.PostfixExpressionPostDecrement p -> noSep [encodePostfixExpression p, cst "--"]

encodeSubscriptOperation :: Cpp.SubscriptOperation -> A.Expr
encodeSubscriptOperation (Cpp.SubscriptOperation array index) =
  noSep [
    encodePostfixExpression array,
    cst "[",
    encodeExpression index,
    cst "]"]

encodeFunctionCallOperation :: Cpp.FunctionCallOperation -> A.Expr
encodeFunctionCallOperation (Cpp.FunctionCallOperation func args) =
  noSep [
    encodePostfixExpression func,
    parens $ commaSep inlineStyle $ encodeExpression <$> args]

encodeTemplateFunctionCallOperation :: Cpp.TemplateFunctionCallOperation -> A.Expr
encodeTemplateFunctionCallOperation (Cpp.TemplateFunctionCallOperation func templateArgs args) =
  noSep [
    encodePostfixExpression func,
    angleBracesList inlineStyle $ encodeTemplateArgument <$> templateArgs,
    parens $ commaSep inlineStyle $ encodeExpression <$> args]

encodeMemberAccessOperation :: Cpp.MemberAccessOperation -> A.Expr
encodeMemberAccessOperation (Cpp.MemberAccessOperation obj member) =
  noSep [encodePostfixExpression obj, cst ".", cst member]

encodePointerMemberAccessOperation :: Cpp.PointerMemberAccessOperation -> A.Expr
encodePointerMemberAccessOperation (Cpp.PointerMemberAccessOperation ptr member) =
  noSep [encodePostfixExpression ptr, cst "->", cst member]

encodePrimaryExpression :: Cpp.PrimaryExpression -> A.Expr
encodePrimaryExpression e = case e of
  Cpp.PrimaryExpressionIdentifier id -> cst id
  Cpp.PrimaryExpressionLiteral l -> encodeLiteral l
  Cpp.PrimaryExpressionParenthesized p -> parens $ encodeExpression p
  Cpp.PrimaryExpressionLambda l -> encodeLambdaExpression l

encodeLambdaExpression :: Cpp.LambdaExpression -> A.Expr
encodeLambdaExpression (Cpp.LambdaExpression captures params retType body) =
  spaceSep [
    encodeCaptureList captures,
    if null params
      then parens $ cst ""
      else parens $ commaSep inlineStyle $ encodeParameter <$> params,
    case retType of
      Just t -> spaceSep [cst "->", encodeTypeExpression t]
      Nothing -> cst "",
    encodeCompoundStatement body]

encodeCaptureList :: Cpp.CaptureList -> A.Expr
encodeCaptureList cl = case cl of
  Cpp.CaptureListCaptureByValue -> cst "[=]"
  Cpp.CaptureListCaptures cs -> bracketList inlineStyle $ encodeCapture <$> cs

encodeCapture :: Cpp.Capture -> A.Expr
encodeCapture (Cpp.Capture name byRef) =
  if byRef
    then cst $ "&" ++ name
    else cst name

-- Pattern matching via visitor pattern
encodePatternMatch :: Cpp.PatternMatch -> A.Expr
encodePatternMatch (Cpp.PatternMatch visitor variant) =
  spaceSep [
    cst "std::visit",
    parens $ commaSep inlineStyle [encodeVisitor visitor, encodeExpression variant]]

encodeVisitor :: Cpp.Visitor -> A.Expr
encodeVisitor v = case v of
  Cpp.VisitorLambda l -> encodeLambdaExpression l
  Cpp.VisitorOverloaded o -> encodeOverloadedLambdas o

encodeOverloadedLambdas :: Cpp.OverloadedLambdas -> A.Expr
encodeOverloadedLambdas (Cpp.OverloadedLambdas lambdas) =
  spaceSep [
    cst "overloaded",
    curlyBlock fullBlockStyle $ newlineSep $ encodeLambdaExpression <$> lambdas]

encodeFunctionApplication :: Cpp.FunctionApplication -> A.Expr
encodeFunctionApplication (Cpp.FunctionApplication func args) =
  spaceSep [
    encodeFunctionIdentifier func,
    parens $ commaSep inlineStyle $ encodeExpression <$> args]

encodeFunctionIdentifier :: Cpp.FunctionIdentifier -> A.Expr
encodeFunctionIdentifier f = case f of
  Cpp.FunctionIdentifierSimple name -> cst name
  Cpp.FunctionIdentifierQualified q -> encodeQualifiedIdentifier q

encodeQualifiedIdentifier :: Cpp.QualifiedIdentifier -> A.Expr
encodeQualifiedIdentifier (Cpp.QualifiedIdentifier ns name) =
  cst $ ns ++ "::" ++ name

-- Literals
encodeLiteral :: Cpp.Literal -> A.Expr
encodeLiteral l = case l of
  Cpp.LiteralInteger i -> encodeIntegerLiteral i
  Cpp.LiteralFloating f -> cst $ show f
  Cpp.LiteralCharacter (Cpp.CharacterLiteral c) -> cst $ "'" ++ c ++ "'"
  Cpp.LiteralString (Cpp.StringLiteral s) -> cst $ "\"" ++ s ++ "\""
  Cpp.LiteralBoolean (Cpp.BooleanLiteral b) -> if b then cst "true" else cst "false"
  Cpp.LiteralNull -> cst "nullptr"

encodeIntegerLiteral :: Cpp.IntegerLiteral -> A.Expr
encodeIntegerLiteral i = case i of
  Cpp.IntegerLiteralDecimal n -> cst $ show n
  Cpp.IntegerLiteralHexadecimal h -> cst $ "0x" ++ h
  Cpp.IntegerLiteralOctal o -> cst $ "0" ++ o
  Cpp.IntegerLiteralBinary b -> cst $ "0b" ++ b

-- Container types
encodeVector :: Cpp.Vector -> A.Expr
encodeVector (Cpp.Vector elemType elems) =
  spaceSep [
    cst "std::vector<",
    encodeTypeExpression elemType,
    cst ">",
    curlyBracesList Nothing inlineStyle $ encodeExpression <$> elems]

encodeMap :: Cpp.Map -> A.Expr
encodeMap (Cpp.Map keyType valType entries) =
  spaceSep [
    cst "std::map<",
    commaSep inlineStyle [encodeTypeExpression keyType, encodeTypeExpression valType],
    cst ">",
    curlyBracesList Nothing inlineStyle $ encodeMapEntry <$> entries]

encodeMapEntry :: Cpp.MapEntry -> A.Expr
encodeMapEntry (Cpp.MapEntry key val) =
  spaceSep [
    curlyBracesList Nothing inlineStyle [encodeExpression key],
    cst "->",
    encodeExpression val]

encodeSet :: Cpp.Set -> A.Expr
encodeSet (Cpp.Set elemType elems) =
  spaceSep [
    cst "std::set<",
    encodeTypeExpression elemType,
    cst ">",
    curlyBracesList Nothing inlineStyle $ encodeExpression <$> elems]

encodeOptional :: Cpp.Optional -> A.Expr
encodeOptional (Cpp.Optional valType val) =
  spaceSep [
    cst "std::optional<",
    encodeTypeExpression valType,
    cst ">",
    case val of
      Just v -> curlyBracesList Nothing inlineStyle [encodeExpression v]
      Nothing -> cst "{}"]

-- Utility functions

toCppComments :: String -> Bool -> String
toCppComments s isMultiline = if isMultiline
  then "/* " ++ s ++ " */"
  else "// " ++ s

encodeComment :: Cpp.Comment -> A.Expr
encodeComment (Cpp.Comment text isMultiline) =
  cst $ toCppComments text isMultiline
