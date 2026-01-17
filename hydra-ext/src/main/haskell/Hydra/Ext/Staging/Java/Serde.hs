-- | Serialization of Java syntax to source code.
--
-- Note: Some syntax node types are stubbed (returning "STUB:...") because they are not
-- currently needed for Hydra's Java code generation. The Java coder generates classes,
-- interfaces, methods, fields, and common expressions, but does not generate constructs
-- like loops, try-catch, switch statements, or arrays. These stubs can be implemented
-- if the coder is extended to generate such constructs.
module Hydra.Ext.Staging.Java.Serde where

import Hydra.Kernel
import Hydra.Serialization
import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Java.Syntax as Java

import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified Data.Char as Chr
import Data.Char (ord, toUpper)
import Numeric (showHex)


sanitizeJavaComment :: String -> String
sanitizeJavaComment s = L.concat (fromChar <$> s)
  where
    fromChar c = case c of
      '<' -> "&lt;"
      '>' -> "&gt;"
      _ -> [c]

singleLineComment :: String -> Ast.Expr
singleLineComment c = cst $ "// " ++ sanitizeJavaComment c

withComments :: Maybe String -> Ast.Expr -> Ast.Expr
withComments mc expr = case mc of
    Nothing -> expr
    Just c -> newlineSep [writeComments c, expr]
  where
    writeComments c = cst $ "/**\n" ++ unlines (toLine <$> (lines $ sanitizeJavaComment c)) ++ " */"
      where
        toLine l = " * " ++ l

writeAdditionalBound :: Java.AdditionalBound -> Ast.Expr
writeAdditionalBound _ = cst "STUB:AdditionalBound"

writeAdditiveExpression :: Java.AdditiveExpression -> Ast.Expr
writeAdditiveExpression e = case e of
  Java.AdditiveExpressionUnary m -> writeMultiplicativeExpression m
  Java.AdditiveExpressionPlus (Java.AdditiveExpression_Binary lhs rhs) ->
    infixWs "+" (writeAdditiveExpression lhs) (writeMultiplicativeExpression rhs)
  Java.AdditiveExpressionMinus (Java.AdditiveExpression_Binary lhs rhs) ->
    infixWs "-" (writeAdditiveExpression lhs) (writeMultiplicativeExpression rhs)

writeAmbiguousName :: Java.AmbiguousName -> Ast.Expr
writeAmbiguousName (Java.AmbiguousName parts) = dotSep (writeIdentifier <$> parts)

writeAndExpression :: Java.AndExpression -> Ast.Expr
writeAndExpression (Java.AndExpression eqs) = infixWsList "&" (writeEqualityExpression <$> eqs)

writeAnnotatedIdentifier :: Java.AnnotatedIdentifier -> Ast.Expr
writeAnnotatedIdentifier (Java.AnnotatedIdentifier anns id) = writeIdentifier id -- Note: ignoring annotations for now

writeAnnotation :: Java.Annotation -> Ast.Expr
writeAnnotation ann = case ann of
  Java.AnnotationNormal n -> writeNormalAnnotation n
  Java.AnnotationMarker m -> writeMarkerAnnotation m
  Java.AnnotationSingleElement s -> writeSingleElementAnnotation s

writeAnnotationTypeDeclaration :: Java.AnnotationTypeDeclaration -> Ast.Expr
writeAnnotationTypeDeclaration _ = cst "STUB:AnnotationTypeDeclaration"

writeArrayAccess :: Java.ArrayAccess -> Ast.Expr
writeArrayAccess _ = cst "STUB:ArrayAccess"

writeArrayCreationExpression :: Java.ArrayCreationExpression -> Ast.Expr
writeArrayCreationExpression _ = cst "STUB:ArrayCreationExpression"

writeArrayInitializer :: Java.ArrayInitializer -> Ast.Expr
writeArrayInitializer _ = cst "STUB:ArrayInitializer"

writeArrayType :: Java.ArrayType -> Ast.Expr
writeArrayType (Java.ArrayType dims variant) = noSep [writeArrayTypeVariant variant, writeDims dims]
  where
    writeArrayTypeVariant v = case v of
      Java.ArrayType_VariantPrimitive pt -> writePrimitiveTypeWithAnnotations pt
      Java.ArrayType_VariantClassOrInterface cit -> writeClassOrInterfaceType cit
      Java.ArrayType_VariantVariable tv -> writeName tv
    writeDims (Java.Dims annsList) = noSep $ fmap writeDim annsList
    writeDim anns = noSep $ Y.catMaybes [
      if L.null anns then Nothing else Just $ spaceSep $ fmap writeAnnotation anns,
      Just $ cst "[]"]

writeAssertStatement :: Java.AssertStatement -> Ast.Expr
writeAssertStatement _ = cst "STUB:AssertStatement"

writeAssignment :: Java.Assignment -> Ast.Expr
writeAssignment (Java.Assignment lhs op rhs) = infixWs ctop (writeLeftHandSide lhs) (writeExpression rhs)
  where
    ctop = case op of
      Java.AssignmentOperatorSimple -> "="
      Java.AssignmentOperatorTimes -> "*="
      Java.AssignmentOperatorDiv -> "/="
      Java.AssignmentOperatorMod -> "%="
      Java.AssignmentOperatorPlus -> "+="
      Java.AssignmentOperatorMinus -> "-="
      Java.AssignmentOperatorShiftLeft -> "<<="
      Java.AssignmentOperatorShiftRight -> ">>="
      Java.AssignmentOperatorShiftRightZeroFill -> ">>>="
      Java.AssignmentOperatorAnd -> "&="
      Java.AssignmentOperatorXor -> "^="
      Java.AssignmentOperatorOr -> "|="

writeAssignmentExpression :: Java.AssignmentExpression -> Ast.Expr
writeAssignmentExpression e = case e of
  Java.AssignmentExpressionConditional c -> writeConditionalExpression c
  Java.AssignmentExpressionAssignment a -> writeAssignment a

writeBlock :: Java.Block -> Ast.Expr
writeBlock (Java.Block stmts) = curlyBlock fullBlockStyle $ newlineSep (writeBlockStatement <$> stmts)

writeBlockStatement :: Java.BlockStatement -> Ast.Expr
writeBlockStatement s = case s of
  Java.BlockStatementLocalVariableDeclaration d -> writeLocalVariableDeclarationStatement d
  Java.BlockStatementClass cd -> writeClassDeclaration cd
  Java.BlockStatementStatement s -> writeStatement s

writeBreakStatement :: Java.BreakStatement -> Ast.Expr
writeBreakStatement (Java.BreakStatement mlabel) = suffixSemi $ spaceSep $ Y.catMaybes [
  Just $ cst "break",
  writeIdentifier <$> mlabel]

writeCastExpression :: Java.CastExpression -> Ast.Expr
writeCastExpression e = case e of
  Java.CastExpressionPrimitive p -> writeCastExpression_Primitive p
  Java.CastExpressionNotPlusMinus npm -> writeCastExpression_NotPlusMinus npm
  Java.CastExpressionLambda l -> writeCastExpression_Lambda l

writeCastExpression_Lambda :: Java.CastExpression_Lambda -> Ast.Expr
writeCastExpression_Lambda _ = cst "STUB:CastExpression_Lambda"

writeCastExpression_NotPlusMinus :: Java.CastExpression_NotPlusMinus -> Ast.Expr
writeCastExpression_NotPlusMinus (Java.CastExpression_NotPlusMinus rb ex) = spaceSep [
  writeCastExpression_RefAndBounds rb,
  writeUnaryExpression ex]

writeCastExpression_RefAndBounds :: Java.CastExpression_RefAndBounds -> Ast.Expr
writeCastExpression_RefAndBounds (Java.CastExpression_RefAndBounds rt adds) = parenList False [spaceSep $ Y.catMaybes [
  Just $ writeReferenceType rt,
  if L.null adds then Nothing else Just $ spaceSep (writeAdditionalBound <$> adds)]]

writeCastExpression_Primitive :: Java.CastExpression_Primitive -> Ast.Expr
writeCastExpression_Primitive _ = cst "STUB:CastExpression_Primitive"

writeCharacterLiteral :: Int -> Ast.Expr
writeCharacterLiteral c = cst $ "'" ++ escapeChar (Chr.chr c) ++ "'"
  where
    escapeChar ch = case ch of
      '\'' -> "\\'"
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ -> if ord ch >= 32 && ord ch < 127
           then [ch]
           else "\\u" ++ L.take (4 - L.length hex) "0000" ++ hex
             where hex = showHex (ord ch) ""
    ord = Chr.ord

writeClassBody :: Java.ClassBody -> Ast.Expr
writeClassBody (Java.ClassBody decls) = curlyBlock fullBlockStyle $
  doubleNewlineSep (writeClassBodyDeclarationWithComments <$> decls)

writeClassBodyDeclaration :: Java.ClassBodyDeclaration -> Ast.Expr
writeClassBodyDeclaration d = case d of
  Java.ClassBodyDeclarationClassMember d -> writeClassMemberDeclaration d
  Java.ClassBodyDeclarationInstanceInitializer i -> writeInstanceInitializer i
  Java.ClassBodyDeclarationStaticInitializer i -> writeStaticInitializer i
  Java.ClassBodyDeclarationConstructorDeclaration d -> writeConstructorDeclaration d

writeClassBodyDeclarationWithComments :: Java.ClassBodyDeclarationWithComments -> Ast.Expr
writeClassBodyDeclarationWithComments (Java.ClassBodyDeclarationWithComments d mc) = withComments mc $
  writeClassBodyDeclaration d

writeClassDeclaration :: Java.ClassDeclaration -> Ast.Expr
writeClassDeclaration d = case d of
  Java.ClassDeclarationNormal nd -> writeNormalClassDeclaration nd
  Java.ClassDeclarationEnum ed -> writeEnumDeclaration ed

writeClassInstanceCreationExpression :: Java.ClassInstanceCreationExpression -> Ast.Expr
writeClassInstanceCreationExpression (Java.ClassInstanceCreationExpression mqual e) = case mqual of
  Nothing -> writeUnqualifiedClassInstanceCreationExpression e
  Just q -> dotSep [writeClassInstanceCreationExpression_Qualifier q, writeUnqualifiedClassInstanceCreationExpression e]

writeClassInstanceCreationExpression_Qualifier :: Java.ClassInstanceCreationExpression_Qualifier -> Ast.Expr
writeClassInstanceCreationExpression_Qualifier q = case q of
  Java.ClassInstanceCreationExpression_QualifierExpression en -> writeExpressionName en
  Java.ClassInstanceCreationExpression_QualifierPrimary p -> writePrimary p

writeClassLiteral :: Java.ClassLiteral -> Ast.Expr
writeClassLiteral _ = cst "STUB:ClassLiteral"

writeClassMemberDeclaration :: Java.ClassMemberDeclaration -> Ast.Expr
writeClassMemberDeclaration d = case d of
  Java.ClassMemberDeclarationField fd -> writeFieldDeclaration fd
  Java.ClassMemberDeclarationMethod md -> writeMethodDeclaration md
  Java.ClassMemberDeclarationClass cd -> writeClassDeclaration cd
  Java.ClassMemberDeclarationInterface id -> writeInterfaceDeclaration id
  Java.ClassMemberDeclarationNone -> semi

writeClassModifier :: Java.ClassModifier -> Ast.Expr
writeClassModifier m = case m of
  Java.ClassModifierAnnotation ann -> writeAnnotation ann
  Java.ClassModifierPublic -> cst "public"
  Java.ClassModifierProtected -> cst "protected"
  Java.ClassModifierPrivate -> cst "private"
  Java.ClassModifierAbstract -> cst "abstract"
  Java.ClassModifierStatic -> cst "static"
  Java.ClassModifierFinal -> cst "final"
  Java.ClassModifierStrictfp -> cst "strictfp"

writeClassOrInterfaceType :: Java.ClassOrInterfaceType -> Ast.Expr
writeClassOrInterfaceType cit = case cit of
  Java.ClassOrInterfaceTypeClass ct -> writeClassType ct
  Java.ClassOrInterfaceTypeInterface it -> writeInterfaceType it

writeClassOrInterfaceTypeToInstantiate :: Java.ClassOrInterfaceTypeToInstantiate -> Ast.Expr
writeClassOrInterfaceTypeToInstantiate (Java.ClassOrInterfaceTypeToInstantiate ids margs) =
  noSep $ Y.catMaybes [
    Just $ dotSep (writeAnnotatedIdentifier <$> ids),
    writeTypeArgumentsOrDiamond <$> margs]

writeClassType :: Java.ClassType -> Ast.Expr
writeClassType (Java.ClassType anns qual id args) = noSep $ Y.catMaybes [
    Just $ spaceSep $ Y.catMaybes [
      if L.null anns then Nothing else Just $ commaSep inlineStyle (writeAnnotation <$> anns),
      Just qualifiedId],
    if L.null args then Nothing else Just $ angleBracesList inlineStyle (writeTypeArgument <$> args)]
  where
    qualifiedId = case qual of
      Java.ClassTypeQualifierNone -> writeTypeIdentifier id
      Java.ClassTypeQualifierPackage pkg -> dotSep [writePackageName pkg, writeTypeIdentifier id]
      Java.ClassTypeQualifierParent cit -> dotSep [writeClassOrInterfaceType cit, writeTypeIdentifier id]

writeCompilationUnit :: Java.CompilationUnit -> Ast.Expr
writeCompilationUnit u = case u of
  Java.CompilationUnitOrdinary (Java.OrdinaryCompilationUnit mpkg imports types) -> doubleNewlineSep $ Y.catMaybes
      [warning, pkgSec, importsSec, typesSec]
    where
      warning = Just $ singleLineComment warningAutoGeneratedFile
      pkgSec = fmap writePackageDeclaration mpkg
      importsSec = if L.null imports
        then Nothing
        else Just $ newlineSep (writeImportDeclaration <$> imports)
      typesSec = if L.null types
        then Nothing
        else Just $ doubleNewlineSep (writeTypeDeclarationWithComments <$> types)

writeConditionalAndExpression :: Java.ConditionalAndExpression -> Ast.Expr
writeConditionalAndExpression (Java.ConditionalAndExpression ors)
  = infixWsList "&&" (writeInclusiveOrExpression <$> ors)

writeConditionalExpression :: Java.ConditionalExpression -> Ast.Expr
writeConditionalExpression c = case c of
  Java.ConditionalExpressionSimple co -> writeConditionalOrExpression co
  Java.ConditionalExpressionTernaryCond tc -> writeConditionalExpression_TernaryCond tc
  Java.ConditionalExpressionTernaryLambda tl -> writeConditionalExpression_TernaryLambda tl

writeConditionalExpression_TernaryCond :: Java.ConditionalExpression_TernaryCond -> Ast.Expr
writeConditionalExpression_TernaryCond _ = cst "STUB:ConditionalExpression_TernaryCond"

writeConditionalExpression_TernaryLambda :: Java.ConditionalExpression_TernaryLambda -> Ast.Expr
writeConditionalExpression_TernaryLambda _ = cst "STUB:ConditionalExpression_TernaryLambda"

writeConditionalOrExpression :: Java.ConditionalOrExpression -> Ast.Expr
writeConditionalOrExpression (Java.ConditionalOrExpression ands)
  = infixWsList "||" (writeConditionalAndExpression <$> ands)

writeConstantDeclaration :: Java.ConstantDeclaration -> Ast.Expr
writeConstantDeclaration (Java.ConstantDeclaration mods typ vars) = suffixSemi $ spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeConstantModifier <$> mods),
  Just $ writeUnannType typ,
  Just $ commaSep inlineStyle (writeVariableDeclarator <$> vars)]

writeConstantModifier :: Java.ConstantModifier -> Ast.Expr
writeConstantModifier _ = cst "STUB:ConstantModifier"

writeConstructorBody :: Java.ConstructorBody -> Ast.Expr
writeConstructorBody (Java.ConstructorBody minvoc stmts) = curlyBlock fullBlockStyle $ doubleNewlineSep $ Y.catMaybes [
  writeExplicitConstructorInvocation <$> minvoc,
  Just $ newlineSep (writeBlockStatement <$> stmts)]

writeConstructorDeclaration :: Java.ConstructorDeclaration -> Ast.Expr
writeConstructorDeclaration (Java.ConstructorDeclaration mods cons mthrows body) = spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeConstructorModifier <$> mods),
  Just $ writeConstructorDeclarator cons,
  writeThrows <$> mthrows,
  Just $ writeConstructorBody body]

writeConstructorDeclarator :: Java.ConstructorDeclarator -> Ast.Expr
writeConstructorDeclarator (Java.ConstructorDeclarator tparams name mrecparam fparams) = spaceSep $ Y.catMaybes [
  if L.null tparams then Nothing else Just $ angleBracesList inlineStyle (writeTypeParameter <$> tparams),
  Just $ writeSimpleTypeName name,
  writeReceiverParameter <$> mrecparam,
  Just $ parenList False (writeFormalParameter <$> fparams)]

writeConstructorModifier :: Java.ConstructorModifier -> Ast.Expr
writeConstructorModifier m = case m of
  Java.ConstructorModifierAnnotation ann -> writeAnnotation ann
  Java.ConstructorModifierPublic -> cst "public"
  Java.ConstructorModifierProtected -> cst "protected"
  Java.ConstructorModifierPrivate -> cst "private"

writeContinueStatement :: Java.ContinueStatement -> Ast.Expr
writeContinueStatement (Java.ContinueStatement mlabel) = suffixSemi $ spaceSep $ Y.catMaybes [
  Just $ cst "continue",
  writeIdentifier <$> mlabel]

writeDims :: Java.Dims -> Ast.Expr
writeDims (Java.Dims anns) = noSep (write <$> anns)
  where
    write _ = cst "[]" -- Note: ignoring annotations on dimensions for now

writeDoStatement :: Java.DoStatement -> Ast.Expr
writeDoStatement _ = cst "STUB:DoStatement"

writeElementValue :: Java.ElementValue -> Ast.Expr
writeElementValue ev = case ev of
  Java.ElementValueConditionalExpression c -> writeConditionalExpression c
  Java.ElementValueElementValueArrayInitializer (Java.ElementValueArrayInitializer values) ->
    commaSep inlineStyle (writeElementValue <$> values)
  Java.ElementValueAnnotation ann -> writeAnnotation ann

writeElementValuePair :: Java.ElementValuePair -> Ast.Expr
writeElementValuePair (Java.ElementValuePair k v) = infixWs "=" (writeIdentifier k) (writeElementValue v)

writeEnumDeclaration :: Java.EnumDeclaration -> Ast.Expr
writeEnumDeclaration _ = cst "STUB:EnumDeclaration"

writeEqualityExpression :: Java.EqualityExpression -> Ast.Expr
writeEqualityExpression e = case e of
  Java.EqualityExpressionUnary r -> writeRelationalExpression r
  Java.EqualityExpressionEqual (Java.EqualityExpression_Binary lhs rhs) ->
    infixWs "==" (writeEqualityExpression lhs) (writeRelationalExpression rhs)
  Java.EqualityExpressionNotEqual (Java.EqualityExpression_Binary lhs rhs) ->
    infixWs "!=" (writeEqualityExpression lhs) (writeRelationalExpression rhs)

writeExclusiveOrExpression :: Java.ExclusiveOrExpression -> Ast.Expr
writeExclusiveOrExpression (Java.ExclusiveOrExpression ands) = infixWsList "^" (writeAndExpression <$> ands)

writeExplicitConstructorInvocation :: Java.ExplicitConstructorInvocation -> Ast.Expr
writeExplicitConstructorInvocation _ = cst "STUB:ExplicitConstructorInvocation"

writeExpression :: Java.Expression -> Ast.Expr
writeExpression e = case e of
  Java.ExpressionLambda l -> writeLambdaExpression l
  Java.ExpressionAssignment a -> writeAssignmentExpression a

writeExpressionName :: Java.ExpressionName -> Ast.Expr
writeExpressionName (Java.ExpressionName mqual id) = dotSep $ Y.catMaybes [
  writeAmbiguousName <$> mqual,
  Just $ writeIdentifier id]

writeExpressionStatement :: Java.ExpressionStatement -> Ast.Expr
writeExpressionStatement (Java.ExpressionStatement stmt) = suffixSemi $ writeStatementExpression stmt

writeFieldAccess :: Java.FieldAccess -> Ast.Expr
writeFieldAccess (Java.FieldAccess qual id) = dotSep $ case qual of
  Java.FieldAccess_QualifierPrimary p -> [writePrimary p, writeIdentifier id]
  Java.FieldAccess_QualifierSuper -> [cst "super", writeIdentifier id]
  Java.FieldAccess_QualifierTyped tn -> [writeTypeName tn, cst "super", writeIdentifier id]

writeFieldDeclaration :: Java.FieldDeclaration -> Ast.Expr
writeFieldDeclaration (Java.FieldDeclaration mods typ vars) = suffixSemi $ spaceSep $ Y.catMaybes [
    if L.null mods then Nothing else Just $ spaceSep (writeFieldModifier <$> mods),
    Just $ writeUnannType typ,
    Just $ commaSep inlineStyle (writeVariableDeclarator <$> vars)]

writeFieldModifier :: Java.FieldModifier -> Ast.Expr
writeFieldModifier m = case m of
  Java.FieldModifierAnnotation ann -> writeAnnotation ann
  Java.FieldModifierPublic -> cst "public"
  Java.FieldModifierProtected -> cst "protected"
  Java.FieldModifierPrivate -> cst "private"
  Java.FieldModifierStatic -> cst "static"
  Java.FieldModifierFinal -> cst "final"
  Java.FieldModifierTransient -> cst "transient"
  Java.FieldModifierVolatile -> cst "volatile"

writeFloatingPointLiteral :: Java.FloatingPointLiteral -> Ast.Expr
writeFloatingPointLiteral (Java.FloatingPointLiteral d) = cst $ show d

writeFloatingPointType :: Java.FloatingPointType -> Ast.Expr
writeFloatingPointType ft = cst $ case ft of
  Java.FloatingPointTypeFloat -> "float"
  Java.FloatingPointTypeDouble -> "double"

writeForStatement :: Java.ForStatement -> Ast.Expr
writeForStatement _ = cst "STUB:ForStatement"

writeFormalParameter :: Java.FormalParameter -> Ast.Expr
writeFormalParameter p = case p of
  Java.FormalParameterSimple s -> writeFormalParameter_Simple s
  Java.FormalParameterVariableArity v -> writeVariableArityParameter v

writeFormalParameter_Simple :: Java.FormalParameter_Simple -> Ast.Expr
writeFormalParameter_Simple (Java.FormalParameter_Simple mods typ id) = spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeVariableModifier <$> mods),
  Just $ writeUnannType typ,
  Just $ writeVariableDeclaratorId id]

writeIdentifier :: Java.Identifier -> Ast.Expr
writeIdentifier (Java.Identifier s) = cst s

writeIfThenStatement :: Java.IfThenStatement -> Ast.Expr
writeIfThenStatement (Java.IfThenStatement cond thn) = spaceSep [
  cst "if",
  parenList False [writeExpression cond],
  writeBlock (Java.Block [Java.BlockStatementStatement thn])]

writeIfThenElseStatement :: Java.IfThenElseStatement -> Ast.Expr
writeIfThenElseStatement _ = cst "STUB:IfThenElseStatement"

writeImportDeclaration :: Java.ImportDeclaration -> Ast.Expr
writeImportDeclaration imp = case imp of
  Java.ImportDeclarationSingleType (Java.SingleTypeImportDeclaration tn) -> suffixSemi $
    spaceSep [cst "import", writeTypeName tn]
  Java.ImportDeclarationTypeImportOnDemand d -> cst "STUB:ImportDeclarationTypeImportOnDemand"
  Java.ImportDeclarationSingleStaticImport d -> cst "STUB:ImportDeclarationSingleStaticImport"
  Java.ImportDeclarationStaticImportOnDemand d -> cst "STUB:ImportDeclarationStaticImportOnDemand"

writeInclusiveOrExpression :: Java.InclusiveOrExpression -> Ast.Expr
writeInclusiveOrExpression (Java.InclusiveOrExpression ors)
  = infixWsList "|" (writeExclusiveOrExpression <$> ors)

writeInstanceInitializer :: Java.InstanceInitializer -> Ast.Expr
writeInstanceInitializer _ = cst "STUB:InstanceInitializer"

writeIntegerLiteral :: Java.IntegerLiteral -> Ast.Expr
writeIntegerLiteral (Java.IntegerLiteral i) = cst $ show i

writeIntegralType :: Java.IntegralType -> Ast.Expr
writeIntegralType t = cst $ case t of
  Java.IntegralTypeByte -> "byte"
  Java.IntegralTypeShort -> "short"
  Java.IntegralTypeInt -> "int"
  Java.IntegralTypeLong -> "long"
  Java.IntegralTypeChar -> "char"

writeInterfaceBody :: Java.InterfaceBody -> Ast.Expr
writeInterfaceBody (Java.InterfaceBody decls) = curlyBlock fullBlockStyle $ doubleNewlineSep
  (writeInterfaceMemberDeclaration <$> decls)

writeInterfaceDeclaration :: Java.InterfaceDeclaration -> Ast.Expr
writeInterfaceDeclaration d = case d of
  Java.InterfaceDeclarationNormalInterface n -> writeNormalInterfaceDeclaration n
  Java.InterfaceDeclarationAnnotationType a -> writeAnnotationTypeDeclaration a

writeInterfaceMemberDeclaration :: Java.InterfaceMemberDeclaration -> Ast.Expr
writeInterfaceMemberDeclaration d = case d of
  Java.InterfaceMemberDeclarationConstant c -> writeConstantDeclaration c
  Java.InterfaceMemberDeclarationInterfaceMethod im -> writeInterfaceMethodDeclaration im
  Java.InterfaceMemberDeclarationClass cd -> writeClassDeclaration cd
  Java.InterfaceMemberDeclarationInterface id -> writeInterfaceDeclaration id

writeInterfaceMethodDeclaration :: Java.InterfaceMethodDeclaration -> Ast.Expr
writeInterfaceMethodDeclaration (Java.InterfaceMethodDeclaration mods header body) = spaceSep $ Y.catMaybes [
      if L.null mods then Nothing else Just $ spaceSep (writeInterfaceMethodModifier <$> mods),
      Just $ writeMethodHeader header,
      Just $ writeMethodBody body]

writeInterfaceMethodModifier :: Java.InterfaceMethodModifier -> Ast.Expr
writeInterfaceMethodModifier m = case m of
  Java.InterfaceMethodModifierAnnotation a -> writeAnnotation a
  Java.InterfaceMethodModifierPublic -> cst "public"
  Java.InterfaceMethodModifierPrivate -> cst "private"
  Java.InterfaceMethodModifierAbstract -> cst "abstract"
  Java.InterfaceMethodModifierDefault -> cst "default"
  Java.InterfaceMethodModifierStatic -> cst "static"
  Java.InterfaceMethodModifierStrictfp -> cst "strictfp"

writeInterfaceModifier :: Java.InterfaceModifier -> Ast.Expr
writeInterfaceModifier m = case m of
  Java.InterfaceModifierAnnotation a -> writeAnnotation a
  Java.InterfaceModifierPublic -> cst "public"
  Java.InterfaceModifierProtected -> cst "protected"
  Java.InterfaceModifierPrivate -> cst "private"
  Java.InterfaceModifierAbstract -> cst "abstract"
  Java.InterfaceModifierStatic -> cst "static"
  Java.InterfaceModifierStrictfb -> cst "strictfb"

writeInterfaceType :: Java.InterfaceType -> Ast.Expr
writeInterfaceType (Java.InterfaceType ct) = writeClassType ct

writeLabeledStatement :: Java.LabeledStatement -> Ast.Expr
writeLabeledStatement _ = cst "STUB:LabeledStatement"

writeLambdaBody :: Java.LambdaBody -> Ast.Expr
writeLambdaBody b = case b of
  Java.LambdaBodyExpression e -> writeExpression e
  Java.LambdaBodyBlock b -> writeBlock b

writeLambdaExpression :: Java.LambdaExpression -> Ast.Expr
writeLambdaExpression (Java.LambdaExpression params body) =
  infixWs "->" (writeLambdaParameters params) (writeLambdaBody body)

writeLambdaParameters :: Java.LambdaParameters -> Ast.Expr
writeLambdaParameters p = case p of
  Java.LambdaParametersTuple l -> parenList False (writeLambdaParameters <$> l)
  Java.LambdaParametersSingle id -> writeIdentifier id

writeLeftHandSide :: Java.LeftHandSide -> Ast.Expr
writeLeftHandSide lhs = case lhs of
  Java.LeftHandSideExpressionName en -> writeExpressionName en
  Java.LeftHandSideFieldAccess fa -> writeFieldAccess fa
  Java.LeftHandSideArrayAccess aa -> writeArrayAccess aa

writeLiteral :: Java.Literal -> Ast.Expr
writeLiteral l = case l of
  Java.LiteralNull -> cst "null"
  Java.LiteralInteger il -> writeIntegerLiteral il
  Java.LiteralFloatingPoint fl -> writeFloatingPointLiteral fl
  Java.LiteralBoolean b -> cst $ if b then "true" else "false"
  Java.LiteralCharacter c -> writeCharacterLiteral c
  Java.LiteralString sl -> writeStringLiteral sl

writeLocalVariableDeclaration :: Java.LocalVariableDeclaration -> Ast.Expr
writeLocalVariableDeclaration (Java.LocalVariableDeclaration mods t decls) = spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeVariableModifier <$> mods),
  Just $ writeLocalName t,
  Just $ commaSep inlineStyle (writeVariableDeclarator <$> decls)]

writeLocalVariableDeclarationStatement :: Java.LocalVariableDeclarationStatement -> Ast.Expr
writeLocalVariableDeclarationStatement (Java.LocalVariableDeclarationStatement d) = suffixSemi $ writeLocalVariableDeclaration d

writeLocalName :: Java.LocalVariableType -> Ast.Expr
writeLocalName t = case t of
  Java.LocalVariableTypeType ut -> writeUnannType ut
  Java.LocalVariableTypeVar -> cst "var"

writeMarkerAnnotation :: Java.MarkerAnnotation -> Ast.Expr
writeMarkerAnnotation (Java.MarkerAnnotation tname) = prefixAt $ writeTypeName tname

writeMethodBody :: Java.MethodBody -> Ast.Expr
writeMethodBody b = case b of
  Java.MethodBodyBlock block -> writeBlock block
  Java.MethodBodyNone -> semi

writeMethodDeclaration :: Java.MethodDeclaration -> Ast.Expr
writeMethodDeclaration (Java.MethodDeclaration anns mods header body) = newlineSep $ Y.catMaybes [
    if L.null anns then Nothing else Just $ newlineSep (writeAnnotation <$> anns),
    Just headerAndBody]
  where
    headerAndBody = spaceSep $ Y.catMaybes [
      if L.null mods then Nothing else Just $ spaceSep (writeMethodModifier <$> mods),
      Just $ writeMethodHeader header,
      Just $ writeMethodBody body]

writeMethodDeclarator :: Java.MethodDeclarator -> Ast.Expr
writeMethodDeclarator (Java.MethodDeclarator id rparam params) = noSep [
  writeIdentifier id,
  -- Note: ignoring receiver param for now
  parenList False (writeFormalParameter <$> params)]

writeMethodHeader :: Java.MethodHeader -> Ast.Expr
writeMethodHeader (Java.MethodHeader params result decl mthrows) = spaceSep $ Y.catMaybes [
  if L.null params then Nothing else Just $ angleBracesList inlineStyle (writeTypeParameter <$> params),
  Just $ writeResult result,
  Just $ writeMethodDeclarator decl,
  writeThrows <$> mthrows]

writeMethodInvocation :: Java.MethodInvocation -> Ast.Expr
writeMethodInvocation (Java.MethodInvocation header args) = noSep [headerSec, argSec]
  where
    argSec = parenList True (writeExpression <$> args)
    headerSec = case header of
      Java.MethodInvocation_HeaderSimple mname -> writeMethodName mname
      Java.MethodInvocation_HeaderComplex (Java.MethodInvocation_Complex var targs id) -> case var of
          Java.MethodInvocation_VariantType tname -> dotSep [writeTypeName tname, idSec]
          Java.MethodInvocation_VariantExpression en -> dotSep [writeExpressionName en, idSec]
          Java.MethodInvocation_VariantPrimary p -> dotSep [writePrimary p, idSec]
          Java.MethodInvocation_VariantSuper -> dotSep [super, idSec]
          Java.MethodInvocation_VariantTypeSuper tname -> dotSep [writeTypeName tname, super, idSec]
        where
          super = cst "super"
          idSec = noSep $ Y.catMaybes [
            if L.null targs then Nothing else Just $ angleBracesList inlineStyle (writeTypeArgument <$> targs),
            Just $ writeIdentifier id]

writeMethodModifier :: Java.MethodModifier -> Ast.Expr
writeMethodModifier m = case m of
  Java.MethodModifierAnnotation ann -> writeAnnotation ann
  Java.MethodModifierPublic -> cst "public"
  Java.MethodModifierProtected -> cst "protected"
  Java.MethodModifierPrivate -> cst "private"
  Java.MethodModifierAbstract -> cst "abstract"
  Java.MethodModifierFinal -> cst "final"
  Java.MethodModifierSynchronized -> cst "synchronized"
  Java.MethodModifierNative -> cst "native"
  Java.MethodModifierStrictfb -> cst "strictfb"

writeMethodName :: Java.MethodName -> Ast.Expr
writeMethodName (Java.MethodName id) = writeIdentifier id

writeMethodReference :: Java.MethodReference -> Ast.Expr
writeMethodReference _ = cst "STUB:MethodReference"

writeMultiplicativeExpression :: Java.MultiplicativeExpression -> Ast.Expr
writeMultiplicativeExpression e = case e of
  Java.MultiplicativeExpressionUnary u -> writeUnaryExpression u
  Java.MultiplicativeExpressionTimes (Java.MultiplicativeExpression_Binary lhs rhs) ->
    infixWs "*" (writeMultiplicativeExpression lhs) (writeUnaryExpression rhs)
  Java.MultiplicativeExpressionDivide (Java.MultiplicativeExpression_Binary lhs rhs) ->
    infixWs "/" (writeMultiplicativeExpression lhs) (writeUnaryExpression rhs)
  Java.MultiplicativeExpressionMod (Java.MultiplicativeExpression_Binary lhs rhs) ->
    infixWs "%" (writeMultiplicativeExpression lhs) (writeUnaryExpression rhs)

writeNormalAnnotation :: Java.NormalAnnotation -> Ast.Expr
writeNormalAnnotation (Java.NormalAnnotation tname pairs) = prefixAt $ noSep [
  writeTypeName tname,
  commaSep inlineStyle (writeElementValuePair <$> pairs)]

writeNormalClassDeclaration :: Java.NormalClassDeclaration -> Ast.Expr
writeNormalClassDeclaration (Java.NormalClassDeclaration mods id tparams msuperc superi body) =
    spaceSep $ Y.catMaybes [modSec, classSec, idSec, extendsSec, implementsSec, bodySec]
  where
    modSec = if L.null mods
      then Nothing
      else Just $ spaceSep (writeClassModifier <$> mods)
    classSec = Just $ cst "class"
    idSec = Just $ noSep $ Y.catMaybes [Just $ writeTypeIdentifier id, params]
      where
        params = if L.null tparams
          then Nothing
          else Just $ angleBracesList inlineStyle (writeTypeParameter <$> tparams)
    extendsSec = fmap (\c -> spaceSep [cst "extends", writeClassType c]) msuperc
    implementsSec = if L.null superi
      then Nothing
      else Just $ spaceSep [cst "implements", commaSep inlineStyle (writeInterfaceType <$> superi)]
    bodySec = Just $ writeClassBody body

writeNormalInterfaceDeclaration :: Java.NormalInterfaceDeclaration -> Ast.Expr
writeNormalInterfaceDeclaration (Java.NormalInterfaceDeclaration mods id tparams extends body) =
    spaceSep $ Y.catMaybes [modSec, classSec, idSec, extendsSec, bodySec]
  where
    modSec = if L.null mods
      then Nothing
      else Just $ spaceSep (writeInterfaceModifier <$> mods)
    classSec = Just $ cst "interface"
    idSec = Just $ noSep $ Y.catMaybes [Just $ writeTypeIdentifier id, params]
      where
        params = if L.null tparams
          then Nothing
          else Just $ angleBracesList inlineStyle (writeTypeParameter <$> tparams)
    extendsSec = if L.null extends then Nothing else Just $
      spaceSep [cst "extends", commaSep inlineStyle (writeInterfaceType <$> extends)]
    bodySec = Just $ writeInterfaceBody body

writeNumericType :: Java.NumericType -> Ast.Expr
writeNumericType nt = case nt of
  Java.NumericTypeIntegral it -> writeIntegralType it
  Java.NumericTypeFloatingPoint ft -> writeFloatingPointType ft

writePackageDeclaration :: Java.PackageDeclaration -> Ast.Expr
writePackageDeclaration (Java.PackageDeclaration mods ids) = suffixSemi $ spaceSep $ Y.catMaybes [
    if L.null mods then Nothing else Just $ spaceSep (writePackageModifier <$> mods),
    Just $ spaceSep [cst "package", cst $ L.intercalate "." (Java.unIdentifier <$> ids)]]

writePackageName :: Java.PackageName -> Ast.Expr
writePackageName (Java.PackageName ids) = dotSep (writeIdentifier <$> ids)

writePackageOrTypeName :: Java.PackageOrTypeName -> Ast.Expr
writePackageOrTypeName (Java.PackageOrTypeName ids) = dotSep (writeIdentifier <$> ids)

writePackageModifier :: Java.PackageModifier -> Ast.Expr
writePackageModifier (Java.PackageModifier ann) = writeAnnotation ann

writePostDecrementExpression :: Java.PostDecrementExpression -> Ast.Expr
writePostDecrementExpression _ = cst "STUB:PostDecrementExpression"

writePostIncrementExpression :: Java.PostIncrementExpression -> Ast.Expr
writePostIncrementExpression _ = cst "STUB:PostIncrementExpression"

writePostfixExpression :: Java.PostfixExpression -> Ast.Expr
writePostfixExpression e = case e of
  Java.PostfixExpressionPrimary p -> writePrimary p
  Java.PostfixExpressionName en -> writeExpressionName en
  Java.PostfixExpressionPostIncrement pi -> writePostIncrementExpression pi
  Java.PostfixExpressionPostDecrement pd -> writePostDecrementExpression pd

writePreDecrementExpression:: Java.PreDecrementExpression -> Ast.Expr
writePreDecrementExpression _ = cst "STUB:PreDecrementExpression"

writePreIncrementExpression :: Java.PreIncrementExpression -> Ast.Expr
writePreIncrementExpression _ = cst "STUB:PreIncrementExpression"

writePrimary :: Java.Primary -> Ast.Expr
writePrimary p = case p of
  Java.PrimaryNoNewArray_ n -> writePrimaryNoNewArray n
  Java.PrimaryArrayCreation a -> writeArrayCreationExpression a

writePrimaryNoNewArray :: Java.PrimaryNoNewArray -> Ast.Expr
writePrimaryNoNewArray p = case p of
  Java.PrimaryNoNewArrayLiteral l -> writeLiteral l
  Java.PrimaryNoNewArrayClassLiteral cl -> writeClassLiteral cl
  Java.PrimaryNoNewArrayThis -> cst "this"
  Java.PrimaryNoNewArrayDotThis n -> dotSep [writeTypeName n, cst "this"]
  Java.PrimaryNoNewArrayParens e -> parenList False [writeExpression e]
  Java.PrimaryNoNewArrayClassInstance ci -> writeClassInstanceCreationExpression ci
  Java.PrimaryNoNewArrayFieldAccess fa -> writeFieldAccess fa
  Java.PrimaryNoNewArrayArrayAccess aa -> writeArrayAccess aa
  Java.PrimaryNoNewArrayMethodInvocation mi -> writeMethodInvocation mi
  Java.PrimaryNoNewArrayMethodReference mr -> writeMethodReference mr

writePrimitiveType :: Java.PrimitiveType -> Ast.Expr
writePrimitiveType pt = case pt of
  Java.PrimitiveTypeNumeric nt -> writeNumericType nt
  Java.PrimitiveTypeBoolean -> cst "boolean"

writePrimitiveTypeWithAnnotations :: Java.PrimitiveTypeWithAnnotations -> Ast.Expr
writePrimitiveTypeWithAnnotations (Java.PrimitiveTypeWithAnnotations pt anns) = spaceSep $ Y.catMaybes [
  if L.null anns then Nothing else Just $ spaceSep (writeAnnotation <$> anns),
  Just $ writePrimitiveType pt]

writeReceiverParameter :: Java.ReceiverParameter -> Ast.Expr
writeReceiverParameter _ = cst "STUB:ReceiverParameter"

writeReferenceType :: Java.ReferenceType -> Ast.Expr
writeReferenceType rt = case rt of
  Java.ReferenceTypeClassOrInterface cit -> writeClassOrInterfaceType cit
  Java.ReferenceTypeVariable v -> writeName v
  Java.ReferenceTypeArray at -> writeArrayType at

writeRelationalExpression :: Java.RelationalExpression -> Ast.Expr
writeRelationalExpression e = case e of
  Java.RelationalExpressionSimple s -> writeShiftExpression s
  Java.RelationalExpressionLessThan lt -> writeRelationalExpression_LessThan lt
  Java.RelationalExpressionGreaterThan gt -> writeRelationalExpression_GreaterThan gt
  Java.RelationalExpressionLessThanEqual lte -> writeRelationalExpression_LessThanEqual lte
  Java.RelationalExpressionGreaterThanEqual gte -> writeRelationalExpression_GreaterThanEqual gte
  Java.RelationalExpressionInstanceof i -> writeRelationalExpression_InstanceOf i

writeRelationalExpression_GreaterThan :: Java.RelationalExpression_GreaterThan -> Ast.Expr
writeRelationalExpression_GreaterThan (Java.RelationalExpression_GreaterThan lhs rhs) =
  infixWs ">" (writeRelationalExpression lhs) (writeShiftExpression rhs)

writeRelationalExpression_GreaterThanEqual :: Java.RelationalExpression_GreaterThanEqual -> Ast.Expr
writeRelationalExpression_GreaterThanEqual (Java.RelationalExpression_GreaterThanEqual lhs rhs) =
  infixWs ">=" (writeRelationalExpression lhs) (writeShiftExpression rhs)

writeRelationalExpression_InstanceOf :: Java.RelationalExpression_InstanceOf -> Ast.Expr
writeRelationalExpression_InstanceOf (Java.RelationalExpression_InstanceOf lhs rhs) =
  infixWs "instanceof" (writeRelationalExpression lhs) (writeReferenceType rhs)

writeRelationalExpression_LessThan :: Java.RelationalExpression_LessThan -> Ast.Expr
writeRelationalExpression_LessThan (Java.RelationalExpression_LessThan lhs rhs) =
  infixWs "<" (writeRelationalExpression lhs) (writeShiftExpression rhs)

writeRelationalExpression_LessThanEqual :: Java.RelationalExpression_LessThanEqual -> Ast.Expr
writeRelationalExpression_LessThanEqual (Java.RelationalExpression_LessThanEqual lhs rhs) =
  infixWs "<=" (writeRelationalExpression lhs) (writeShiftExpression rhs)

writeResult :: Java.Result -> Ast.Expr
writeResult r = case r of
  Java.ResultType t -> writeUnannType t
  Java.ResultVoid -> cst "void"

writeReturnStatement :: Java.ReturnStatement -> Ast.Expr
writeReturnStatement (Java.ReturnStatement mex) = suffixSemi $ spaceSep $ Y.catMaybes [
  Just $ cst "return",
  writeExpression <$> mex]

writeShiftExpression :: Java.ShiftExpression -> Ast.Expr
writeShiftExpression e = case e of
  Java.ShiftExpressionUnary a -> writeAdditiveExpression a
  Java.ShiftExpressionShiftLeft (Java.ShiftExpression_Binary lhs rhs) ->
    infixWs "<<" (writeShiftExpression lhs) (writeAdditiveExpression rhs)
  Java.ShiftExpressionShiftRight (Java.ShiftExpression_Binary lhs rhs) ->
    infixWs ">>" (writeShiftExpression lhs) (writeAdditiveExpression rhs)
  Java.ShiftExpressionShiftRightZeroFill (Java.ShiftExpression_Binary lhs rhs) ->
    infixWs ">>>" (writeShiftExpression lhs) (writeAdditiveExpression rhs)

writeSimpleTypeName :: Java.SimpleTypeName -> Ast.Expr
writeSimpleTypeName (Java.SimpleTypeName tid) = writeTypeIdentifier tid

writeSingleElementAnnotation :: Java.SingleElementAnnotation -> Ast.Expr
writeSingleElementAnnotation (Java.SingleElementAnnotation tname mv) = case mv of
  Nothing -> writeMarkerAnnotation (Java.MarkerAnnotation tname)
  Just v -> prefixAt $ noSep [writeTypeName tname, parenList False [writeElementValue v]]

writeStatement :: Java.Statement -> Ast.Expr
writeStatement s = case s of
  Java.StatementWithoutTrailing s -> writeStatementWithoutTrailingSubstatement s
  Java.StatementLabeled l -> writeLabeledStatement l
  Java.StatementIfThen it -> writeIfThenStatement it
  Java.StatementIfThenElse ite -> writeIfThenElseStatement ite
  Java.StatementWhile w -> writeWhileStatement w
  Java.StatementFor f -> writeForStatement f

writeStatementExpression :: Java.StatementExpression -> Ast.Expr
writeStatementExpression e = case e of
  Java.StatementExpressionAssignment ass -> writeAssignment ass
  Java.StatementExpressionPreIncrement pi -> writePreIncrementExpression pi
  Java.StatementExpressionPreDecrement pd -> writePreDecrementExpression pd
  Java.StatementExpressionPostIncrement pi -> writePostIncrementExpression pi
  Java.StatementExpressionPostDecrement pd -> writePostDecrementExpression pd
  Java.StatementExpressionMethodInvocation m -> writeMethodInvocation m
  Java.StatementExpressionClassInstanceCreation cic -> writeClassInstanceCreationExpression cic

writeStatementWithoutTrailingSubstatement :: Java.StatementWithoutTrailingSubstatement -> Ast.Expr
writeStatementWithoutTrailingSubstatement s = case s of
  Java.StatementWithoutTrailingSubstatementBlock b -> writeBlock b
  Java.StatementWithoutTrailingSubstatementEmpty -> semi
  Java.StatementWithoutTrailingSubstatementExpression e -> writeExpressionStatement e
  Java.StatementWithoutTrailingSubstatementAssert a -> writeAssertStatement a
  Java.StatementWithoutTrailingSubstatementSwitch s -> writeSwitchStatement s
  Java.StatementWithoutTrailingSubstatementDo d -> writeDoStatement d
  Java.StatementWithoutTrailingSubstatementBreak b -> writeBreakStatement b
  Java.StatementWithoutTrailingSubstatementContinue c -> writeContinueStatement c
  Java.StatementWithoutTrailingSubstatementReturn r -> writeReturnStatement r
  Java.StatementWithoutTrailingSubstatementSynchronized s -> writeSynchronizedStatement s
  Java.StatementWithoutTrailingSubstatementThrow t -> writeThrowStatement t
  Java.StatementWithoutTrailingSubstatementTry t -> writeTryStatement t

writeStaticInitializer :: Java.StaticInitializer -> Ast.Expr
writeStaticInitializer _ = cst "STUB:StaticInitializer"

writeStringLiteral :: Java.StringLiteral -> Ast.Expr
writeStringLiteral (Java.StringLiteral s) = cst $ "\"" ++ escapeJavaString s ++ "\""
  where
    escapeJavaString = concatMap escapeChar
    escapeChar c = case c of
      '"'  -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      _ | c < ' ' || c > '~' -> "\\u" ++ padHex (ord c)
        | otherwise -> [c]
    padHex n = replicate (4 - length hex) '0' ++ hex
      where hex = fmap toUpper $ showHex n ""

writeSwitchStatement :: Java.SwitchStatement -> Ast.Expr
writeSwitchStatement _ = cst "STUB:SwitchStatement"

writeSynchronizedStatement :: Java.SynchronizedStatement -> Ast.Expr
writeSynchronizedStatement _ = cst "STUB:SynchronizedStatement"

writeThrowStatement :: Java.ThrowStatement -> Ast.Expr
writeThrowStatement (Java.ThrowStatement ex) = suffixSemi $ spaceSep [cst "throw", writeExpression ex]

writeThrows :: Java.Throws -> Ast.Expr
writeThrows _ = cst "STUB:Throws"

writeTryStatement :: Java.TryStatement -> Ast.Expr
writeTryStatement _ = cst "STUB:TryStatement"

writeType :: Java.Type -> Ast.Expr
writeType t = case t of
  Java.TypePrimitive pt -> writePrimitiveTypeWithAnnotations pt
  Java.TypeReference rt -> writeReferenceType rt

writeTypeArgument :: Java.TypeArgument -> Ast.Expr
writeTypeArgument a = case a of
  Java.TypeArgumentReference rt -> writeReferenceType rt
  Java.TypeArgumentWildcard w -> writeWildcard w

writeTypeArgumentsOrDiamond :: Java.TypeArgumentsOrDiamond -> Ast.Expr
writeTypeArgumentsOrDiamond targs = case targs of
  Java.TypeArgumentsOrDiamondArguments args -> angleBracesList inlineStyle (writeTypeArgument <$> args)
  Java.TypeArgumentsOrDiamondDiamond -> cst "<>"

writeTypeBound :: Java.TypeBound -> Ast.Expr
writeTypeBound _ = cst "STUB:TypeBound"

writeTypeDeclaration :: Java.TypeDeclaration -> Ast.Expr
writeTypeDeclaration d = case d of
  Java.TypeDeclarationClass d -> writeClassDeclaration d
  Java.TypeDeclarationInterface d -> writeInterfaceDeclaration d
  Java.TypeDeclarationNone -> semi

writeTypeDeclarationWithComments :: Java.TypeDeclarationWithComments -> Ast.Expr
writeTypeDeclarationWithComments (Java.TypeDeclarationWithComments d mc) = withComments mc $ writeTypeDeclaration d

writeTypeIdentifier :: Java.TypeIdentifier -> Ast.Expr
writeTypeIdentifier (Java.TypeIdentifier id) = writeIdentifier id

writeTypeName :: Java.TypeName -> Ast.Expr
writeTypeName (Java.TypeName id mqual) = dotSep $ Y.catMaybes [
  writePackageOrTypeName <$> mqual,
  Just $ writeTypeIdentifier id]

writeTypeParameter :: Java.TypeParameter -> Ast.Expr
writeTypeParameter (Java.TypeParameter mods id bound) = spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeTypeParameterModifier <$> mods),
  Just $ writeTypeIdentifier id,
  fmap (\b -> spaceSep [cst "extends", writeTypeBound b]) bound]

writeTypeParameterModifier :: Java.TypeParameterModifier -> Ast.Expr
writeTypeParameterModifier (Java.TypeParameterModifier ann) = writeAnnotation ann

writeName :: Java.TypeVariable -> Ast.Expr
writeName (Java.TypeVariable anns id) = spaceSep $ Y.catMaybes [
  if L.null anns then Nothing else Just $ spaceSep (writeAnnotation <$> anns),
  Just $ writeTypeIdentifier id]

writeUnannType :: Java.UnannType -> Ast.Expr
writeUnannType (Java.UnannType t) = writeType t

writeUnaryExpression :: Java.UnaryExpression -> Ast.Expr
writeUnaryExpression e = case e of
  Java.UnaryExpressionPreIncrement pi -> writePreIncrementExpression pi
  Java.UnaryExpressionPreDecrement pd -> writePreDecrementExpression pd
  Java.UnaryExpressionPlus p -> spaceSep [cst "+", writeUnaryExpression p]
  Java.UnaryExpressionMinus m -> spaceSep [cst "-", writeUnaryExpression m]
  Java.UnaryExpressionOther o -> writeUnaryExpressionNotPlusMinus o

writeUnaryExpressionNotPlusMinus :: Java.UnaryExpressionNotPlusMinus -> Ast.Expr
writeUnaryExpressionNotPlusMinus e = case e of
  Java.UnaryExpressionNotPlusMinusPostfix p -> writePostfixExpression p
  Java.UnaryExpressionNotPlusMinusTilde u -> spaceSep [cst "~", writeUnaryExpression u]
  Java.UnaryExpressionNotPlusMinusNot u -> noSep [cst "!", writeUnaryExpression u]
  Java.UnaryExpressionNotPlusMinusCast c -> writeCastExpression c

writeUnqualifiedClassInstanceCreationExpression  :: Java.UnqualifiedClassInstanceCreationExpression -> Ast.Expr
writeUnqualifiedClassInstanceCreationExpression (Java.UnqualifiedClassInstanceCreationExpression targs cit args mbody)
  = spaceSep $ Y.catMaybes [
    Just $ cst "new",
    if L.null targs then Nothing else Just $ angleBracesList inlineStyle (writeTypeArgument <$> targs),
    Just $ noSep [writeClassOrInterfaceTypeToInstantiate cit, parenList False (writeExpression <$> args)],
    writeClassBody <$> mbody]

writeVariableArityParameter :: Java.VariableArityParameter -> Ast.Expr
writeVariableArityParameter _ = cst "STUB:VariableArityParameter"

writeVariableDeclarator :: Java.VariableDeclarator -> Ast.Expr
writeVariableDeclarator (Java.VariableDeclarator id minit) =
    Y.maybe idSec (infixWs "=" idSec . writeVariableInitializer) minit
  where
    idSec = writeVariableDeclaratorId id

writeVariableDeclaratorId :: Java.VariableDeclaratorId -> Ast.Expr
writeVariableDeclaratorId (Java.VariableDeclaratorId id mdims) = noSep $ Y.catMaybes [
  Just $ writeIdentifier id,
  writeDims <$> mdims]

writeVariableInitializer :: Java.VariableInitializer -> Ast.Expr
writeVariableInitializer i = case i of
  Java.VariableInitializerExpression e -> writeExpression e
  Java.VariableInitializerArrayInitializer ai -> writeArrayInitializer ai

writeVariableModifier :: Java.VariableModifier -> Ast.Expr
writeVariableModifier m = case m of
  Java.VariableModifierAnnotation ann -> writeAnnotation ann
  Java.VariableModifierFinal -> cst "final"

writeWhileStatement :: Java.WhileStatement -> Ast.Expr
writeWhileStatement _ = cst "STUB:WhileStatement"

writeWildcard :: Java.Wildcard -> Ast.Expr
writeWildcard (Java.Wildcard anns mbounds) = spaceSep $ Y.catMaybes [
  if L.null anns then Nothing else Just $ commaSep inlineStyle (writeAnnotation <$> anns),
  Just $ cst "*",
  writeWildcardBounds <$> mbounds]

writeWildcardBounds :: Java.WildcardBounds -> Ast.Expr
writeWildcardBounds b = case b of
  Java.WildcardBoundsExtends rt -> spaceSep [cst "extends", writeReferenceType rt]
  Java.WildcardBoundsSuper rt -> spaceSep [cst "super", writeReferenceType rt]

prefixAt :: Ast.Expr -> Ast.Expr
prefixAt e = noSep [cst "@", e]

semi :: Ast.Expr
semi = cst ";"

suffixSemi :: Ast.Expr -> Ast.Expr
suffixSemi e = noSep [e, semi]
